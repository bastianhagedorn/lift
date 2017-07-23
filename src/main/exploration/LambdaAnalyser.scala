package exploration

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import exploration.KernelGenerator.parser
import exploration.ParameterRewrite.{logger, parser, readLambdaFromFile, settings, _}
import exploration.ParameterSearch.SubstitutionMap
import ir.{ArrayType, Size, TypeChecker}
import ir.ast.{Expr, FunCall, Gather, Lambda, ReorderWithStride, Slide, Split}
import lift.arithmetic._
import opencl.executor._
import opencl.generator.NDRange
import opencl.ir.pattern._
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.{ListMap, Map}
import scala.io.Source
import scala.util.Random
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
import ExpressionFilter.Status.Success


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

  private val settingsFile = parser.option[String](List("f", "file"), "name",
    "The settings file to use."
  ) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage(s"Settings file $file doesn't exist.")
      s
  }

  private var settings = Settings()

  def main(args: Array[String]): Unit = {

    parser.parse(args)

    var lambdaPath = input.value.get.toPath.toAbsolutePath.toString

    //new stuff!
    var hlLambdaPath = input.value.get.toPath.getParent.getParent.getParent //Lower dir
    var hlName = hlLambdaPath.getFileName.toString
    hlLambdaPath = hlLambdaPath.getParent
    hlLambdaPath = hlLambdaPath.getParent.resolve(hlLambdaPath.getFileName.toString.dropRight("Lower".length)).resolve(hlName.substring(0, 1)).resolve(hlName.substring(1, 2)).resolve(hlName)
    val settings = ParseSettings(None)


    val high_level_expr_orig = readLambdaFromFile(hlLambdaPath.toString)

    val vars = high_level_expr_orig.getVarsInParams()

    val combinations = settings.inputCombinations

    println("combinations: " + combinations)

    val st =
      if (combinations.isDefined &&
        combinations.get.head.length == vars.length)
        (vars: Seq[ArithExpr], combinations.get.head).zipped.toMap
      else
        createValueMap(high_level_expr_orig)

    val sizesForFilter = st.values.toSeq
    println("sizesForFilter: " + sizesForFilter)


    val high_level_expr = replaceInputTypes(high_level_expr_orig, st)

    TypeChecker(high_level_expr)


    //Weitere Parameter mit Ranges und ggf. Constraints auslesen
    val tunables: Map[String, Map[String, Any]] = getParameterInformation(high_level_expr)



    //herausfinden, in wie vielen Dimensionen getuned werden soll
    val lambdaStr = readFromFile(lambdaPath)

    //println("lambdaStr: " + lambdaStr)
    val lowLevelFactory = Eval.getMethod(lambdaStr)

    //TODO why do we pass 5 empty vars here?
    val lambda = lowLevelFactory(Seq(Var(), Var(), Var(), Var(), Var()))

    /*
    val typeChecker = TypeChecker(lambda)
    //println("typeChecker : " + typeChecker)

    //println("lambda: " + lambda)

    println("params: " + lambda.params)

    var tunableVars = Expr.visitLeftToRight(Set[Var]())(lambda.body, (e, s) =>
      e.t.varList.toSet[Var] ++ s
    ).filterNot(elem => lambda.getVarsInParams() contains elem)
  */


    val numDimension = maxDimension(lambda)
    val dims = (0 to numDimension).toList

    print("Dimension des Kernels: " + numDimension)

    //add the GS and LS to be tuned
    var gsls = (for (dim <- dims) yield ("gs" + dim, Map(("range", Var().range)))).toMap[String, Map[String, Object]]
    gsls = gsls ++ (for (dim <- dims) yield ("ls" + dim, Map(("range", Var().range), ("divides", "gs" + dim)))).toMap[String, Map[String, Object]]
    //gsls = gsls ++ (for (v <- tunableVars) yield (v.toString, Map(("range", v.range)))).toMap[String, Map[String, Object]]

    //generate json from vars:
    var jsonList = List[JSONObject]()
    (gsls ++ tunables).foreach(
      v => {
        //never used...
        //val range = if(v._2==RangeUnknown) RangeAdd(1,1025,1) else v._2

        var fields = ListMap[String, Any](
          ("name", v._1.toString)
        )

        v._2.foreach(
          restrictions => {
            fields = if (restrictions._2.isInstanceOf[Range]) {
              val range = if ((restrictions._2.asInstanceOf[Range]) == RangeUnknown) RangeAdd(1, 1025, 1) else (restrictions._2.asInstanceOf[Range])
              fields + (("interval", JSONObject(
                ListMap[String, Any](
                  ("type", "int"),
                  ("from", range.min),
                  ("to", range.max))
              ))
                )
            } else fields + ((restrictions._1, restrictions._2))
          }
        )
        val newJSONObject = JSONObject(fields)
        jsonList = newJSONObject :: jsonList
      }
    )

    val output = new File(input.value.get.getParentFile, input.value.get.getName + ".json")
    println(output)
    val writer = new PrintWriter(new FileWriter(output, false))
    try writer.write(JSONArray(jsonList).toString()) finally writer.close()

    println("endJson: " + JSONArray(jsonList).toString())

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

  private def maxDimension(expr: Lambda): Int = {
    var usedDimensions: Set[Int] = Set()
    Expr.visit(expr.body, {
      case FunCall(MapGlb(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapLcl(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapWrg(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapAtomLcl(dim, _, _), _) =>
        usedDimensions += dim

      case FunCall(MapAtomWrg(dim, _, _), _) =>
        usedDimensions += dim

      case _ =>
    }, (_) => Unit)

    println("used Dimensions: " + usedDimensions)

    usedDimensions.max
  }

  private def getParameterInformation(lambda: Lambda): Map[String, Map[String, Object]] = {
    val tunableNodes = Utils.findTunableNodes(lambda)

    // from that, isolate only the splits/slides
    val splits = tunableNodes.collect({
      case FunCall(Split(cs), x) => (cs, x.t.asInstanceOf[ArrayType with Size].size)
      // step has to divide len - (size - step)
      case FunCall(Slide(size, step), x) => (step, x.t.asInstanceOf[ArrayType with Size].size - (size-step))
      case FunCall(Gather(ReorderWithStride(s)), x) if s.isInstanceOf[Var] => (s, x.t.asInstanceOf[ArrayType with Size].size)
    })

    // TODO not quite sure but I think it's better to use a mutable map here and make it immutable later on.
    // Otherwise we have to create a new map eacht time we want to "insert" a value.
    // maybe the way to go is simply not using foreach?
    val result = scala.collection.mutable.Map.empty[String,Map[String,Object]]
    splits.foreach(split =>
      split match {
        // If the dividend is a constant, create range and divides constraint based on that value.
        case (v:Var, Cst(dividend)) =>
          result += v.toString -> Map[String, Object](
            "range"->RangeAdd(1,dividend,1),
            "divides" -> dividend.toString
          )
        // If the dividend is a variable, create a divides constraint which implicitly limits the range to the range of the dividend.
        case (v: Var, dividend: Var) =>
          result += v.toString -> Map[String, Object](
            "range"->RangeAdd(1,dividend,1),
            "divides" -> dividend.toString
          )
        case (x,y) =>
          println(s"Not a tunable $x, $y")
      }
    )


    result.toMap
  }
}