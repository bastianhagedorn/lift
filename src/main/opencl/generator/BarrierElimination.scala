package opencl.generator

import apart.arithmetic.{?, ArithExpr, Cst, IntDiv}
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

/**
 * A pass for eliminating unnecessary barriers.
 *
 * Barriers are only necessary when the same memory locations get reused (they are in
 * a loop) or if different threads try to read memory locations other threads write to.
 *
 * The pass visits all sub-expressions and determines where splits, joins and reorders
 * take place (they cause threads to interact) and whether the sub-expressions appear
 * inside loops (causes reuse of memory locations).
 *
 */
object BarrierElimination {

  /**
   * Visit the lambda and determine which barriers can be eliminated.
   *
   * @param lambda The starting lambda.
   */
  def apply(lambda: Lambda): Unit = {
    apply(lambda.body, insideLoop = false)
  }

  private def apply(expr: Expr, insideLoop: Boolean): Unit = {
    expr match {
      case call: FunCall =>

        val calls = getCallsAtLevel(call)
        markLevel(calls, insideLoop)

        calls.foreach(call => call.f match {
          case m: AbstractMap => apply(m.f.body, insideLoop || isLoop(m.iterationCount))
          case r: AbstractPartRed => apply(r.f.body, insideLoop || isLoop(r.iterationCount))
          case i: Iterate => apply(i.f.body, insideLoop || isLoop(i.iterationCount))
          case toLocal(f) => apply(insideLoop, f)
          case toGlobal(f) => apply(insideLoop, f)
          case toPrivate(f) => apply(insideLoop, f)
          case fp: FPattern => apply(fp.f.body, insideLoop)
          case _ =>
        })

      case _ =>
    }
  }

  // Helper method to bypass toAddressSpace and not visit whatever is inside twice
  private def apply(insideLoop: Boolean, f: Lambda1): Unit = {
    f.body match {
      case call: FunCall => call.f match {
        case fp: FPattern => apply(fp.f.body, insideLoop)
      }
      case _ =>
    }
  }

  private def getCallsAtLevel(expr: Expr): Seq[FunCall] = {
    expr match {
      case call: FunCall =>
        call +: call.args.reverse.flatMap(getCallsAtLevel)
      case _ => Seq()
    }
  }

  private def isLoop(ae: ArithExpr) =
    !(ae == Cst(1) || ae == Cst(0) || ae == IntDiv(1, ?))

  private def markLevel(calls: Seq[FunCall], insideLoop: Boolean): Unit = {

    val c = calls.count(_.isConcrete(false))

    var next = calls
    var groups = Seq[Seq[FunCall]]()

    if (c > 0) {

      // Partition the functions into groups such that the last element
      // of a group is concrete, except possibly in the last group
      while (next.nonEmpty) {
        val prefixLength = next.prefixLength(_.isAbstract(false))
        groups = groups :+ next.take(prefixLength + 1)
        next = next.drop(prefixLength + 1)
      }

      // If it is not concrete, then there can't be a barrier
      if (groups.last.last.isAbstract)
        groups = groups.init

      val needsBarrier = Array.fill(groups.length)(false)

      val barrierInHead = groups.head.exists(c => isMapLcl(c.f))
      val finalReadMemory = readsFrom(groups.head.last)
      needsBarrier(0) =
        if (groups.length > 1)
          !(barrierInHead && (finalReadMemory == GlobalMemory || finalReadMemory == LocalMemory && !insideLoop))
        else
          (OpenCLMemory.containsLocalMemory(groups.head.last.mem) ||
            groups.head.last.args.foldLeft(false)((needsBarrier, arg) => {
              OpenCLMemory.containsLocalMemory(arg.mem) || needsBarrier
            })) && insideLoop

      groups.zipWithIndex.foreach(x => {
        val group = x._1
        val id = x._2

        // Conservative assumption. TODO: Not if only has matching splits and joins
        if (group.exists(l => l.f.isInstanceOf[Split] || l.f.isInstanceOf[Join] && id > 0)) {
          needsBarrier(id) = true

          // Split/Join in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Scatter affects the writing of this group and therefore the reading of the
        // group before. Gather in init affects the reading of the group before
        if (group.exists(_.f.isInstanceOf[Scatter]) || group.init.exists(_.f.isInstanceOf[Gather])
          || group.exists(c => c.f.isInstanceOf[Transpose] || c.f.isInstanceOf[TransposeW])) {
          needsBarrier(id) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Gather in last affects the reading of this group
        if (group.last.f.isInstanceOf[Gather] && id < groups.length - 1) {
          needsBarrier(id + 1) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (readsFrom(group.last) == LocalMemory && id > 0 && insideLoop &&
            !groups.slice(0, id).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id) = true
        }
      })

      (groups, needsBarrier).zipped.foreach((group, valid) => if (!valid) invalidateBarrier(group))
    }
  }

  private def isMapLcl(funDecl: FunDecl): Boolean = {
    def isMapLclLambda(f: Lambda1): Boolean = {
      f.body match {
        case call: FunCall => isMapLcl(call.f)
        case _ => false
      }
    }

    funDecl match {
      case _: MapLcl => true
      case toLocal(f) => isMapLclLambda(f)
      case toGlobal(f) => isMapLclLambda(f)
      case toPrivate(f) => isMapLclLambda(f)
      case _ => false
    }
  }

  private def invalidateBarrier(group: Seq[FunCall]): Unit = {
    group.find(c => isMapLcl(c.f)) match {
      case Some(b) => invalidateBarrier(b.f)
      case None =>
    }
  }

  private def invalidateBarrier(declaration: FunDecl): Unit = {
    def invalidateInToAddress(f: Lambda1): Unit = {
      f.body match {
        case call: FunCall => invalidateBarrier(Seq(call))
        case _ =>
      }
    }
    declaration match {
      case ml: MapLcl => ml.emitBarrier = false
      case toGlobal(f) => invalidateInToAddress(f)
      case toLocal(f) => invalidateInToAddress(f)
      case toPrivate(f) => invalidateInToAddress(f)
      case _ =>
    }
  }

  private def readsFrom(call: FunCall): OpenCLAddressSpace = {
    val result = Expr.visitWithState[OpenCLAddressSpace](UndefAddressSpace)(call, (expr, addressSpace) => {
      expr match {
        case call: FunCall =>
          call.f match {
            case uf: UserFun =>
              if (addressSpace == UndefAddressSpace)
                OpenCLMemory.asOpenCLMemory(call.args.head.mem).addressSpace
              else
                addressSpace
            case _ => addressSpace
          }
        case _ => addressSpace
      }
    }, visitArgs = false)

    result
  }

}
