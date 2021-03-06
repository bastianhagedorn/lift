package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir._

/**
 * Transpose pattern. Performs the transpose on the next read.
 * Code for this pattern can be generated.
 *
 * Equivalent to Split(N) o Gather(IndexFunction.transposeFunction(M, N)) o Join() when applied to type
 * ArrayType(ArrayType( ..., M), N) but infers N and M automatically during view generation.
 *
 * The transpose pattern has the following high-level semantics:
 * `Transpose()([ [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,], ..., [z,,1,,, ..., z,,n,,] ]) = [ [x,,1,,, y,,1,,, ..., z,,1,,], [x,,2,,, y,,2,,, ..., z,,2,,], ..., [x,,n,,, y,,n,, ..., z,,n,,,] ]`
 *
 * The transpose pattern has the following type:
 * `Transpose() : [ [a],,I,, ],,J,, -> [ [a],,J,, ],,I,,`
 */
case class Transpose() extends Pattern(arity = 1) {


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(ArrayTypeWSWC(t, ns,nc), ms,mc) => ArrayTypeWSWC(ArrayTypeWSWC(t, ms,mc), ns,nc)
      case _ => throw new TypeException(argType, "ArrayType(ArrayType(_,_),_)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)
    args.head match {
      case vec: Vector[Vector[_] @unchecked] => vec.transpose
    }
  }
}
