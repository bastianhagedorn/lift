package cgoSearch

import apart.arithmetic.{ArithExpr, Cst, Var}
import exploration.utils.Utils
import ir.ArrayType
import ir.ast._

import scala.collection.immutable.Map

object ParameterSearch {
  // A substitution map is a collection of var/value pairs
  type SubstitutionMap = Map[ArithExpr, ArithExpr]

  // A substitution table represents all valid substitution maps
  type SubstitutionTable = List[SubstitutionMap]

  private def propagate(splits: List[(ArithExpr, ArithExpr)],
                m: Map[ArithExpr, ArithExpr]): List[(ArithExpr, ArithExpr)] =
    splits.map((x) => (ArithExpr.substitute(x._1, m), ArithExpr.substitute(x._2, m)))

  // recursively build the substitution table.
  // It takes the first node to tune and recurse with all its possible values.
  private def substitute(splits: List[(ArithExpr, ArithExpr)],
                 substitutions: SubstitutionMap,
                 table: SubstitutionTable): SubstitutionTable = {

    if (splits.nonEmpty) {
      splits.head match {
        // If the stride is not set and the input length is constant, compute all divisors
        case (v: Var, Cst(len)) =>
          ((if (len == 1) 1 else 2) to (if (len == 1) len else len - 1)).filter(len % _ == 0).foldLeft(table)((table, x) =>
            substitute(propagate(splits.tail, Map(v -> x)), substitutions + (v -> x), table))

        // If the input AND the stride are already set, make sure they are multiple
        case (Cst(chunk), Cst(len)) if len % chunk == 0 =>
          substitute(splits.tail, substitutions, table)

        // Otherwise we cannot set the parameter or the current combination is invalid
        case (x, y) =>
          println(s"failed: $x, $y")
          table
      }
    }
    else // if we propagated all the nodes, we have a (hopefully valid) substitution table
      substitutions :: table
  }

  /**
   * Return all the possible parameter sets for a given expression.
   *
   * @param lambda The lambda to build substitutions for.
   * @return Table of valid substitutions.
   */
  def apply(lambda: Lambda): SubstitutionTable = {
    // find all the nodes using variables
    val tunableNodes = Utils.findTunableNodes(lambda)

    // from that, isolate only the splits
    val splits = tunableNodes.collect({
      case FunCall(Split(cs), x) => (cs, x.t.asInstanceOf[ArrayType].len)
      case FunCall(Gather(ReorderWithStride(s)), x) if s.isInstanceOf[Var] => (s, x.t.asInstanceOf[ArrayType].len)
    })

    substitute(splits, Map.empty, List.empty)
  }
}
