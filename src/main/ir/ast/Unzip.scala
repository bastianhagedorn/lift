package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir.{TypeException, TupleType, ArrayType, Type}

/**
 * Unzip pattern.
 * Code for this pattern can be generated.
 *
 * The unzip pattern has the following high-level semantics:
 * `Unzip()( [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]) = ([x,,1,,, ..., x,,n,,], [y,,1,, , ..., y,,n,,] )`
 * The definitions for `n > 2` are accordingly.
 *
 * The unzip pattern has the following type:
 * `Unzip() : [a x b],,i,, -> ([a],,i,,, [b],,i,,)`
 * The definitions for `n > 2` are accordingly.
 */
case class Unzip() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(tt: TupleType, n) =>
        TupleType( tt.elemsT.map(t => ArrayType(t, n)):_* )

      case _ => throw new TypeException(argType, "ArrayType(TupleType, _)")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case i: Iterator[(_, _)]    =>
        val vec = i.toVector.unzip[Any, Any]
        (vec._1.iterator, vec._2.iterator): (Iterator[_], Iterator[_])

      case i: Iterator[(_, _, _)] =>
        val vec = i.toVector.unzip3[Any, Any, Any]
        (vec._1.iterator, vec._2.iterator, vec._3.iterator): (Iterator[_], Iterator[_], Iterator[_])

      case _ => throw new NotImplementedError()
    }
  }
}

