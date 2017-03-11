package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class Array2DFromUserFunGenerator(f: UserFun,
                                       override val at: ArrayType) extends ArrayConstructors(at) {
  override def copy: Expr = Array2DFromUserFunGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    val n = at.len.eval
    //    Array.tabulate(n)( i => f(i, n) )
    ???
  }

}