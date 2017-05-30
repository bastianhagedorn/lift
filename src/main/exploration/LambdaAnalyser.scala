package exploration

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import exploration.KernelGenerator.parser
import exploration.ParameterRewrite.readFromFile
import ir.TypeChecker
import ir.ast.{Expr, Lambda}
import lift.arithmetic.{?, ArithExpr, Var}
import opencl.executor._
import opencl.generator.NDRange
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Random
import scala.util.parsing.json
import scala.util.parsing.json.JSONObject



/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object LambdaAnalyser {

  private val logger = Logger(this.getClass)

  private val parser = new ArgotParser("ParameterRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, _) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }



  def main(args: Array[String]): Unit = {

    parser.parse(args)

    val inputArgument = input.value.get

    var lambdaPath = Paths.get(inputArgument).toAbsolutePath.toString
    val lambdaStr = readFromFile(lambdaPath)

    println("lambdaStr: " + lambdaStr)
    val lowLevelFactory = Eval.getMethod(lambdaStr)

    val lambda = lowLevelFactory(Seq(Var(), Var()))

    val typeChecker = TypeChecker(lambda)
    println("typeChecker : " + typeChecker)

    println("lambda: " + lambda)

    println("params: " + lambda.params)

    val tunableNodes = Utils.findTunableNodes(lambda)
    println("tunableNode: " + tunableNodes)


    var tunableVars = Expr.visitLeftToRight(Set[Var]())(lambda.body, (e, s) =>
      e.t.varList.toSet[Var] ++ s
    ).filterNot(elem => lambda.getVarsInParams() contains elem)

    println("tunables: " + tunableVars)


    //JSON generieren
    val interval = ListMap[String,Any](("type","int"), ("from", "1"), ("to", "1024"))
    val tunableVars2 = ListMap[String,Any](("name","v__1"), ("interval", JSONObject(interval)))

    //var lm = ListMap[String,JSONObject]()
    //lm+=("tunables" -> JSONObject(tunableVars2))

    print("endJson: " + JSONObject(tunableVars2).toString())


  }

  def readLambdaFromFile(filename: String) =
    Eval(readFromFile(filename))

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString

  def writeToFile(filePath: String, s: String): Unit = {
    val file = new File(filePath)
    //file.mkdirs()
    val fw = new FileWriter(file, false)
    val pw = new PrintWriter(fw)
    try pw.write(s) finally pw.close()
  }
}
