package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import exploration.ParameterSearch.SubstitutionMap
import ir.ast.{Expr, FunCall, Lambda}
import ir.{Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval
import opencl.generator.NDRange
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting.InferNDRange
import rewriting.utils.Utils
import ExpressionFilter.Status.Success

import scala.collection.immutable.Map
import scala.io.Source

/**
  * This main currently sets the LS and GS of the lowLevelExpression
  */
object TunerRewrite {

  private val logger = Logger(this.getClass)

  private var topFolder = ""

  private val parser = new ArgotParser("TunerRewrite")

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

  private val localSize = parser.option[Int](List("localSize"), "n",
    "")

  private val globalSize = parser.option[Int](List("globalSize"), "n",
    "")

  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)

      val inputArgument = input.value.get

      topFolder = Paths.get(inputArgument).toString

      logger.info(s"Arguments: ${args.mkString(" ")}")

      // list all the high level expression
      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList
      val highLevelCount = all_files.size

      val parentFolder = Paths.get(topFolder).toAbsolutePath.getParent

      var expr_counter = 0
      all_files.foreach(filename => {

        val fullFilename = parentFolder + "/" + filename

        if (Files.exists(Paths.get(fullFilename))) {
          val high_level_hash = filename.split("/").last
          expr_counter = expr_counter + 1
          println(s"High-level expression : $expr_counter / $highLevelCount")

          try {

            val high_level_expr_orig = readLambdaFromFile(fullFilename)

            val vars = high_level_expr_orig.getVarsInParams()

            //TODO das muss wohl auch noch anders
            val st = createValueMap(high_level_expr_orig)
            val sizesForFilter = st.values.toSeq
            val high_level_expr = replaceInputTypes(high_level_expr_orig, st)

            TypeChecker(high_level_expr)

            val all_substitution_tables: Seq[SubstitutionMap] = ParameterSearch(high_level_expr)
            val substitutionCount = all_substitution_tables.size
            println(s"Found $substitutionCount valid parameter sets")

            val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
            if (Files.exists(Paths.get(loweredIndex))
              && substitutionCount < 800000) {

              val low_level_expr_list = Source.fromFile(loweredIndex).getLines().toList

              val low_level_counter = new AtomicInteger()
              val lowLevelCount = low_level_expr_list.size
              val propagation_counter = new AtomicInteger()
              val propagationCount = lowLevelCount * substitutionCount
              println(s"Found $lowLevelCount low level expressions")

              val parList = low_level_expr_list.par

              parList.foreach(low_level_filename => {

                try {

                  val low_level_hash = low_level_filename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + low_level_filename
                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  println(s"Low-level expression ${low_level_counter.incrementAndGet()} / $lowLevelCount")
                  println("Propagating parameters...")

                  val potential_expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                    all_substitution_tables.flatMap(st => {

                      println(s"\rPropagation ${propagation_counter.incrementAndGet()} / $propagationCount")
                      val params = st.toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                      try {
                        val expr = low_level_factory(sizesForFilter ++ params)
                        TypeChecker(expr)

                        //Genau hier muss man wohl dran.
                        val rangeList = Seq(InferNDRange(expr))

                        logger.debug(rangeList.length + " generated NDRanges")

                        val filtered: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                          rangeList.flatMap {ranges =>
                            if (ExpressionFilter(expr, ranges, settings.searchParameters) == Success)
                              Some((low_level_factory(vars ++ params), params, ranges))
                            else
                              None
                          }

                        logger.debug(filtered.length + " NDRanges after filtering")
                        val sampled = filtered

                        val sampleStrings: Seq[String] = sampled.map(x => low_level_hash + "_" + x._2.mkString("_") +
                          "_" + x._3._1.toString.replace(",", "_") + "_" + x._3._2.toString.replace(",", "_"))
                        logger.debug("\nSampled NDRanges:\n\t" + sampleStrings.mkString(" \n "))
                        Some(sampled)

                      } catch {
                        case _: ir.TypeException => None

                        //noinspection SideEffectsInMonadicTransformation
                        case x: Throwable =>
                          logger.warn("Failed parameter propagation", x)
                          logger.warn(low_level_hash)
                          logger.warn(params.mkString("; "))
                          logger.warn(low_level_str)
                          None
                      }
                    }).flatten

                  println(s"Generating ${potential_expressions.size} kernels")

                  val hashes = SaveOpenCL(topFolder, low_level_hash,
                    high_level_hash, settings, potential_expressions)

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    logger.warn(t.toString)
                }
              })
            }
          } catch {
            case t: Throwable =>
              logger.warn(t.toString)
          }
        }
      })
    }
    catch {
      case io: IOException =>
        logger.error(io.toString)
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString

  def readLambdaFromFile(filename: String) =
    Eval(readFromFile(filename))

  def createValueMap(lambda: Lambda, sizes: Seq[ArithExpr] = Seq()): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.getVarsInParams()

    val actualSizes: Seq[ArithExpr] = sizes

    (vars, actualSizes).zipped.toMap
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

 }
