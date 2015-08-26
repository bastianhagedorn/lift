package exploration

import ir._
import ir.ast._

object Rewrite {

  def getExprForId(expr: Expr, id: Int, idMap: collection.Map[Expr, Int]): Expr =
    idMap.find(pair => pair._2 == id).get._1

  def applyRuleAtId(lambda: Lambda, id: Int, rule: Rule): Lambda = {
    val replacement = applyRuleAtId(lambda.body, id, rule)
    Lambda(lambda.params, replacement)
  }

  def applyRuleAtId(expr: Expr, id: Int, rule: Rule): Expr = {
    val numbering = NumberExpression.breadthFirst(expr)
    applyRuleAtId(expr, id, rule, numbering)

  }

  def depthFirstApplyRuleAtId(expr:Expr, id: Int, rule: Rule): Expr = {
    val numbering = NumberExpression.depthFirst(expr)
    applyRuleAtId(expr, id, rule, numbering)
  }

  def applyRuleAtId(expr: Expr, id: Int, rule: Rule, numbering: collection.Map[Expr, Int]): Expr = {
    TypeChecker.check(expr)
    Context.updateContext(expr)
    val toBeReplaced = getExprForId(expr, id, numbering)
    Expr.replace(expr, toBeReplaced, rule.rewrite(toBeReplaced))
  }


  private[exploration] def listAllPossibleRewritesForRules(lambda: Lambda, rules: Seq[Rule]): Seq[(Rule, Int)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private[exploration] def listAllPossibleRewrites(lambda: Lambda,
                                      rule: Rule): Seq[(Rule, Int)] = {
    Context.updateContext(lambda.body)

    val numbering = NumberExpression.breadthFirst(lambda)

    Expr.visitWithState(Seq[(Rule, Int)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e)) {
        s :+ (rule, numbering(e))
      } else s
    })
  }



  def rewrite(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAtId(lambda, ruleAt._2, ruleAt._1))

    // TODO: Not all generable kernels are valid...
    val (g, notG) = rewritten.partition( _.isGenerable )

    if (levels == 1) {
      g
    } else {
      g ++ notG.flatMap( l => rewrite(l, rules, levels-1))
    }
  }

  def rewrite(lambda: Lambda, levels: Int = 1): Seq[Lambda] =
    rewrite(lambda, allRules, levels)

}


