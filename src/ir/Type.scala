package ir

// hm ...
import opencl.ir._
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

case class TypeException(msg: String) extends Exception(msg) {
  def this() = this("")
  def this(found: Type, expected: String) = this(found + " found but " + expected + " expected")
  def this(found: Type, expected: Type) = this(found + " found but " + expected + " expected")

}


sealed abstract class Type

case class ScalarType(val name: String, val size: Expr) extends Type {
  override def toString = name
}

// TODO: Is the VectorType OpenCL specific? If yes -> move to opencl.ir package
case class VectorType(val scalarT: ScalarType, val len: Expr) extends Type

case class TupleType(val elemsT: Type*) extends Type {
  override def toString = "(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"
}

case class ArrayType(val elemT: Type, val len: Expr) extends Type {
  override def toString = elemT + "[" + len + "]"
}

//case class UnboundArrayType(et: Type, te: TypeExpr) extends ArrayType(et)
//case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type {override def toString = "UndefType"}

object Type {
  
  /*def visitExpr(t: Type, pre: (Expr) => (Unit), post: (Expr) => (Unit)) : Unit = {    
    t match {
      case at: ArrayType => {
        pre(at.len) 
        visitExpr(at.elemT, pre, post)
        post(at.len)
      }
      case tt: TupleType => tt.elemsT.map(et => visitExpr(et,pre,post))              
      case _ => //throw new NotImplementedError()
    }
  } */

  def visitRebuild(t: Type, pre: (Type) => (Type), post: (Type) => (Type)) : Type = {
    var newT = pre(t)
    newT = newT match {
      case at: ArrayType => new ArrayType(visitRebuild(at.elemT, pre, post), at.len)
      case vt: VectorType => new VectorType(visitRebuild(vt.scalarT, pre, post).asInstanceOf[ScalarType],vt.len)
      case tt: TupleType => new TupleType(tt.elemsT.map(et => visitRebuild(et,pre,post)):_*)
      case _ => newT // nothing to do
    }
    post(newT)
  }

  def visit(t: Type, pre: (Type) => (Unit), post: (Type) => (Unit)) : Unit = {
    pre(t)
    t match {
      case at: ArrayType => visit(at.elemT, pre, post)
      case vt: VectorType => visit(vt.scalarT, pre, post)      
      case tt: TupleType => tt.elemsT.map(et => visit(et,pre,post))
      case _ => // nothing to do
    }
    post(t)
  }  
  
  def getElemT(t: Type): Type = {
    t match {
      case at: ArrayType => at.elemT
      case vt: VectorType => vt.scalarT
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
  
  def getLength(t: Type) : Expr = {
    t match {
      case at: ArrayType => at.len
      case st: ScalarType => Cst(1)
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
  
  def getSizeInBytes(t: Type) : Expr = {
    ExprSimplifier.simplify(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
        case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
        case _ => throw new TypeException(t, "??")
      }
    )
  }

  private def asScalar(at0: ArrayType): Type = {
    at0.elemT match {
      case vt:VectorType => new ArrayType(vt.scalarT,at0.len*vt.len)
      case at:ArrayType =>  new ArrayType(asScalar(at),at0.len)
      case _ => throw new TypeException(at0.elemT , "ArrayType or VectorType")
    }
  }
  
  private def asVector(at0: ArrayType, len: Expr): Type = {
    at0.elemT match {      
      case pt:ScalarType => new ArrayType(new VectorType(pt,len), at0.len/len)
      case at1:ArrayType => new ArrayType(asVector(at1,len), at0.len)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }
  
  def length(t: Type, array: Array[Expr] = Array.empty[Expr]) : Array[Expr] = {
    t match {
      case ArrayType(elemT, len) => Type.length(elemT, array :+ len)
      case TupleType(_) => throw new TypeException(t, "ArrayType")
      case VectorType(_, _) => throw new TypeException(t, "ArrayType") // TODO: Think about what to do with vector types
      case _ => array
    }
  }

  def reifyExpr(e1: Expr, e2: Expr) : immutable.Map[TypeVar, Expr] = {
    val result = mutable.Map[TypeVar, Expr]()
    (e1,e2) match {
      case (tv: TypeVar, _) => result += (tv->e2)
      case (_, tv: TypeVar) => result += (tv->e1)
      case _ => // todo check that the two expressions are equivalent
    }
    result.toMap
  }

  // Throw an exception if the types does not match
  def reify(t1: Type, t2: Type): immutable.Map[TypeVar, Expr] = {
    val result = mutable.Map[TypeVar, Expr]()
    (t1, t2) match {
      case (at1: ArrayType, at2: ArrayType) => result ++= reifyExpr(at1.len, at2.len) ++= reify(at1.elemT, at2.elemT)
      case (tt1: TupleType, tt2: TupleType) => result ++= tt1.elemsT.zip(tt2.elemsT).foldLeft(mutable.Map[TypeVar, Expr]())((map, types) => map ++= reify(types._1, types._2))
      case (vt1: VectorType, vt2: VectorType) => result ++= reifyExpr(vt1.len, vt2.len)
      case _ => if (t1 != t2) throw new TypeException(t1, t2)
    }
    result.toMap
  }

  def substitute(t: Type, substitutions: immutable.Map[Expr,Expr]) : Type = {
    Type.visitRebuild(t, t1 => t1, t1 => {
      t1 match {
        case ArrayType(et,len) => new ArrayType(et, Expr.substitute(len, substitutions.toMap))
        case VectorType(st,len) => new VectorType(st, Expr.substitute(len, substitutions.toMap))
        case _ => t1
      }

    })
  }

 /* def isSubtype(top: Type, sub: Type) : Boolean = {
    (top,sub) match {
      case (tat : ArrayType, sat : ArrayType) => {
        val tatLen = ExprSimplifier.simplify(tat.len)
        val satlen = ExprSimplifier.simplify(sat.len)
        val lenIsSubtype = tatLen match {
          case TypeVar(_) => true
          case _ => (tatLen == satlen)
        }
        if (!lenIsSubtype)
          false
        else
          isSubtype(tat.elemT,sat.elemT)
      }
      case (ttt:TupleType, stt:TupleType) =>
        val pairs = ttt.elemsT.zip(stt.elemsT)
        pairs.foldLeft(true)((result,pair) => if (!isSubtype(pair._1,pair._2)) return false else true)

      case _ => top == sub
    }
  }*/

  private def closedFormIterate(inT: Type, ouT: Type, n: Expr) : Type = {
    (inT,ouT) match {
      case (inAT : ArrayType, outAT : ArrayType) => {

        val closedFormLen = {
          val inLen = ExprSimplifier.simplify(inAT.len)
          val outLen = ExprSimplifier.simplify(outAT.len)
          if (inLen == outLen)
            return ouT

          inLen match {
            case tv: TypeVar => {

              // recognises a*tv
              val a = ExprSimplifier.simplify(outLen / tv)
              if (!Expr.contains(a, tv)) {
                // we have a*tv where tv is not present inside a
                Pow(a, n)
              }
              else throw new TypeException("Cannot infer closed form for iterate return type (only support x*a). inT = " + inT + " ouT = " + ouT)
            }
            case _ => throw new TypeException("Cannot infer closed form for iterate return type. inT = " + inT + " ouT = " + ouT)
          }
        }

        new ArrayType(closedFormIterate(inAT.elemT, outAT.elemT, n),closedFormLen)

      }
      case (inTT:TupleType, outTT:TupleType) =>
        new TupleType(inTT.elemsT.zip(outTT.elemsT).map({case (tIn,tOut) => closedFormIterate(tIn,tOut,n)} ) :_*)

      case _ => if (inT == ouT) ouT else throw new TypeException("Cannot infer closed form for iterate return type. inT = "+inT+" ouT = "+ouT)
    }
  }
  
  def check(f: Fun) : Type = { check(f, UndefType) }
  
  def check(f: Fun, inT: Type, setType: Boolean = true): Type = {

    if (setType)
      f.inT = inT // set the input type

    // type inference
    var inferredOuT = f match {
                  
      case AbstractMap(inF) =>
        val elemT = getElemT(inT)
        ArrayType(check(inF, elemT), getLength(inT))

      
      case AbstractReduce(inF) =>
        val elemT = getElemT(inT)
        check(inF, TupleType(elemT, elemT)) // TODO change this probably
        ArrayType(elemT, new Cst(1))

      
      case PartRed(inF) =>
        // TODO: check id !? 
        new ArrayType(getElemT(inT),?)
      
      case cf: CompFun =>
        cf.funs.last.inT = inT
        cf.funs.foldRight(inT)((f, inputT) => check(f, inputT))

      case _:Join => inT match {
        case at0: ArrayType => at0.elemT match {
          case at1: ArrayType => ArrayType(at1.elemT, at0.len * at1.len)
          case _=>  throw new TypeException(at0.elemT, "ArrayType")
        }
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case Split(cs) => inT match {
        case at: ArrayType => ArrayType(ArrayType(at.elemT,cs), at.len / cs)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case _:asScalar  => inT match {
        case at: ArrayType => asScalar(at)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }

      case asVector(len) => inT match {
        case at: ArrayType => asVector(at, len)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case uf : UserFun => {
        val substitutions = reify(uf.expectedInT, inT)
        substitute(uf.expectedOutT, substitutions.toMap)
      }
      
      case input : Input => input.expectedOutT

      case tL:toLocal => check(tL.f, inT)

      case tG:toGlobal => check(tG.f, inT)

      case i : Iterate => inT match {
        case at: ArrayType => {

          // TODO: do not assign the type of i.f (or make somehow independent on the iteration count)
          // check that the input type of iterate can be used as an input for f
          // val ouT1 = check(i.f, inT, false)

          // check that the output type of f can be used as an input for f
          // val ouT2 = check(i.f, i.f.ouT, false)

          // substitute all the expression in the input type with type variables
          val tvMap = scala.collection.mutable.HashMap[TypeVar, Expr]()
          val inputTypeWithTypeVar = visitRebuild(at, t => t, t =>
            t match {
              case at: ArrayType => {
                val tv = TypeVar()
                tvMap += tv -> at.len
                new ArrayType(at.elemT, tv)
              }
              case vt: VectorType => {
                val tv = TypeVar()
                tvMap += tv -> vt.len
                new VectorType(vt.scalarT, tv)
              }
              case _ => t
            }
          )

          // type checking
          var outputTypeWithTypeVar = check(i.f, inputTypeWithTypeVar, false)

          // find all the type variable in the output type
          val outputTvSet = scala.collection.mutable.HashSet[TypeVar]()
          visit(outputTypeWithTypeVar, t => {}, {
              case at: ArrayType  => outputTvSet ++= Expr.getTypeVars(at.len)
              case vt: VectorType => outputTvSet ++= Expr.getTypeVars(vt.len)
              case _ =>
            }
          )

          // put back the expression when the type variable is not present
          tvMap --= outputTvSet
          outputTypeWithTypeVar = substitute(outputTypeWithTypeVar, tvMap.toMap)

          outputTypeWithTypeVar = visitRebuild(outputTypeWithTypeVar, t => t, t =>
            t match {
              case at: ArrayType => new ArrayType(at.elemT, Expr.substitute(at.len, tvMap.toMap))
              case vt: VectorType => new VectorType(vt.scalarT, Expr.substitute(vt.len, tvMap.toMap))
              case _ => t
            }
          )

          // assign the type for f
          check(i.f, inputTypeWithTypeVar)

          //if (isSubtype(inputTypeWithTypeVar, outputTypeWithTypeVar)) {
            closedFormIterate(inputTypeWithTypeVar, outputTypeWithTypeVar, i.n)
          //} else
          //  // TODO: implement support for shape change when the number of iteration is a constant
          //  throw new TypeException("Cannot deal with function inside iterate that change the shape of the type (function = "+i.f+")")

          // TODO: CD to implement logic to automatically infer the return type in the general case (not only for reduction)

          /*val inN = getLength(i.f.inT)
          val outN = getLength(i.f.ouT)

          // compute i.n times a over b (b is the reduction factor of one iteration, a is the input length)
          var a = inN
          var b = inN / outN
          for (index <- 1 to Expr.toInt(i.n)) {
            a = ExprSimplifier.simplify(a / b)
          }

          new ArrayType(getElemT(inT), a)*/
        }
        case _ => throw new TypeException(inT, "ArrayType")
      }

      case _: ReorderStride => inT

      case vec: Vectorize => check(vec.f, inT) // Type.vectorize(vec.n, inT)

      // Type.vectorize(vec.n, inT)

      case NullFun => inT // TODO: change this
      
      // TODO: continue
      //case _ => UndefType
    }


    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, ExprSimplifier.simplify(len))
      case _ => inferredOuT
    }

    if (setType)
      f.ouT = inferredOuT

    inferredOuT
  }

  /*
  def vectorize(n: Expr, t: Type): Type = {
    t match {
      case sT: ScalarType => new VectorType(sT, n)
      case tT: TupleType => new TupleType( tT.elemsT.map( vectorize(n, _) ):_* )
      case aT: ArrayType => asVector(aT, n)
      case _ => throw new TypeException(t, "anything else")
    }
  }
  */

}