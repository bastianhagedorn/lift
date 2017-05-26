package exploration

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import exploration.KernelGenerator.parser
import exploration.ParameterRewrite.readFromFile
import ir.TypeChecker
import ir.ast.Lambda
import lift.arithmetic.ArithExpr
import opencl.executor._
import opencl.generator.NDRange
import org.clapper.argot._

import scala.io.Source
import scala.util.Random
import scala.util.parsing.json


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
    val lambdaStr = readLambdaFromFile(lambdaPath)

    println("lambdaStr: " + lambdaStr)
    //val lowLevelFactory = Eval.getMethod(lambdaStr)



    val test = TypeChecker(lambdaStr)
    println("test: " + test)


    //val tunableVars = Expr.visitLeftToRight(Set[Var]())(lambda.body, (e, s) =>
    //  e.t.varList.toSet[Var] ++ s
    //).filterNot(elem => lambda.getVarsInParams() contains elem)


    //var tuningWerte = vars.value.getOrElse(Seq.empty[ArithExpr]).toArray
    //     Fehler Abfangen und Nutzer mitteilen, das die Anzahl der Parameter nicht stimmt.
    //val lambda = lowLevelFactory(tuningWerte)

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
