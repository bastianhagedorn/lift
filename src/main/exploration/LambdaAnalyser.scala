package exploration

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import exploration.KernelGenerator.parser
import exploration.ParameterRewrite.readFromFile
import ir.TypeChecker
import ir.ast.{Expr, Lambda}
import lift.arithmetic.{?, ArithExpr, Range, RangeAdd, RangeUnknown, Var}
import opencl.executor._
import opencl.generator.NDRange
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Random
import scala.util.parsing.json.JSONArray
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

  private val input = parser.parameter[File]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      file
  }



  def main(args: Array[String]): Unit = {

    parser.parse(args)

    var lambdaPath = input.value.get.toPath.toAbsolutePath.toString
    val lambdaStr = readFromFile(lambdaPath)

    //println("lambdaStr: " + lambdaStr)
    val lowLevelFactory = Eval.getMethod(lambdaStr)

    val lambda = lowLevelFactory(Seq(Var(), Var()))

    val typeChecker = TypeChecker(lambda)
    //println("typeChecker : " + typeChecker)

    //println("lambda: " + lambda)

    println("params: " + lambda.params)

    val tunableNodes = Utils.findTunableNodes(lambda)
    println("tunableNode0: " + tunableNodes(0))


    var tunableVars = Expr.visitLeftToRight(Set[Var]())(lambda.body, (e, s) =>
      e.t.varList.toSet[Var] ++ s
    ).filterNot(elem => lambda.getVarsInParams() contains elem)

    println("tunables: " + tunableVars)

    var tunableVarList = tunableVars.toList

    //JSON aus vars generieren
    var jsonList = List[JSONObject]()

    tunableVars.foreach(
      v=>{
        println("name:"+v.name)
        println(v.toString)
        val range = if(v.range==RangeUnknown) RangeAdd(1,1025,1) else v.range
        jsonList = JSONObject(ListMap[String,Any](
          ("name",v.toString),
          ("interval", JSONObject(
            ListMap[String,Any](
              ("type","int"),
              ("from", range.min),
              ("to", range.max)
            )
          ))
        ))::jsonList
      }
    )

    val output = new File(input.value.get.getParentFile,input.value.get.getName+".json")
    println(output)
    val writer = new PrintWriter(new FileWriter(output,false))
    try writer.write(JSONArray(jsonList).toString()) finally writer.close()



    //var lm = ListMap[String,JSONObject]()
    //lm+=("tunables" -> JSONObject(tunableVars2))

    print("endJson: " + JSONArray(jsonList).toString())


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
