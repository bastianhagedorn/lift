package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import exploration.ExpressionFilter.Status.Success
import exploration.ParameterSearch.SubstitutionMap
import exploration.TunerRewrite.parser
import ir.ast.{Expr, FunCall, Lambda}
import ir.{Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval
import opencl.generator.NDRange
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.Map
import scala.io.Source
import scala.sys.process._
import scala.util.Random

/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object OldKernelGenerator {

  private val logger = Logger(this.getClass)

  private var topFolder = ""

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

  private val localSize = parser.option[NDRange](List("ls", "localSize"), "n",
    "Comma separated local sizes"){
    (s,opt)=>
      //take the first 3 values, try to convert them to number and fill ones if there were less than 3 values
      val localSizes = s.split(',').take(3).map(x=>x.toInt).padTo(3,1)
      NDRange(localSizes(0),localSizes(1),localSizes(2))
  }

  private val globalSize = parser.option[NDRange](List("gs", "globalSize"), "n",
    "Comma separated local sizes"){
    (s,opt)=>
      val localSizes = s.split(',').take(3).map(x=>x.toInt).padTo(3,1)
      NDRange(localSizes(0),localSizes(1),localSizes(2))
  }


  private val sequential = parser.flag[Boolean](List("s", "seq", "sequential"),
    "Don't execute in parallel.")

  private val generateScala = parser.flag[Boolean](List("generate-scala"),
    "Generate lambdas in Scala as well as in OpenCL")

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

  private var lambdaFilename = ""

  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)

      val inputArgument = input.value.get

      //we need the relative path from topfolder to the lambda
      var lambdaPath = Paths.get(inputArgument).toAbsolutePath
      var relativeLambdaPath = lambdaPath.toString.stripPrefix(lambdaPath.getParent.getParent.getParent.getParent.getParent + "/")

      var highLevelName = Paths.get(inputArgument).toAbsolutePath.getParent.getParent.getParent.getFileName

      topFolder = Paths.get(inputArgument).toAbsolutePath.getParent.getParent.getParent.getParent.toString.stripSuffix("Lower")


      lambdaFilename = topFolder + "Scala/lambdaFile"
      if (generateScala.value.isDefined) {
        val f = new File(lambdaFilename)
        if (f.exists()) {
          f.delete()
        } else {
          s"mkdir -p ${topFolder}Scala".!
        }
      }

      settings = ParseSettings(settingsFile.value)

      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info(s"Settings:\n$settings")

      // list all the high level expression
      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList

      val parentFolder = Paths.get(topFolder).toAbsolutePath.getParent

      var filename = ""
      //find corresponding HighLevel Exp.
      all_files.foreach(path => {
        if (Paths.get(path.mkString).getFileName.toString == highLevelName.toString) {
          filename = path
          logger.info(s"corresponding HighLevelExp at path: $filename")
        }
      })

        //path to the HighLevelExp.
        val fullFilename = parentFolder + "/" + filename

        if (Files.exists(Paths.get(fullFilename))) {
          val high_level_hash = filename.split("/").last

          try {

            //HighLevelExpr einlesen
            val high_level_expr_orig = readLambdaFromFile(fullFilename)
            val vars = high_level_expr_orig.getVarsInParams()

            val combinations = settings.inputCombinations

            val st =
              if (combinations.isDefined &&
                  combinations.get.head.length == vars.length)
                (vars: Seq[ArithExpr], combinations.get.head).zipped.toMap
              else
                createValueMap(high_level_expr_orig)

            val sizesForFilter = st.values.toSeq

            val high_level_expr = replaceInputTypes(high_level_expr_orig, st)

            TypeChecker(high_level_expr)

            var all_substitution_tables: Seq[SubstitutionMap] = ParameterSearch(high_level_expr)

            all_substitution_tables = all_substitution_tables.dropRight(all_substitution_tables.size - 1)

            val substitutionCount = all_substitution_tables.size
            println(s"Found $substitutionCount valid parameter sets")


            println("all_substitution_tables: " + all_substitution_tables)

              val propagation_counter = new AtomicInteger()
              val propagationCount = substitutionCount

              var low_level_filename = relativeLambdaPath//parList.last
                try {

                  val low_level_hash = low_level_filename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + low_level_filename
                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  println("Propagating parameters...")

                  val potential_expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                    all_substitution_tables.flatMap(st => {


                      println(s"\rPropagation ${propagation_counter.incrementAndGet()} / $propagationCount")

                      //Hier stehen die Parameter drin
                      val params = st.toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                      println("st: " + st)

                      println("params: " + params)

                      try {
                        val expr = low_level_factory(sizesForFilter ++ params)

                        println("expr: " + expr)

                        TypeChecker(expr)

                        val rangeList = Seq((localSize.value.getOrElse(NDRange(1,1,1)),globalSize.value.getOrElse(NDRange(1,1,1))))

                        logger.debug(rangeList.length + " entered NDRanges")


                        val sampled: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                          rangeList.flatMap {ranges =>
                            //No filtering necessary, we trust Atf to find good & valid parameters
                            Some((low_level_factory(vars ++ params), params, ranges))
                          }

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
                        logger.warn(settings.searchParameters.defaultSize.toString)
                        None
                    }
                  }).flatten

                  println("potential_expressions: " + potential_expressions)


                  println(s"Generating ${potential_expressions.size} kernels")

                  val hashes = SaveOpenCL(topFolder, low_level_hash,
                    high_level_hash, settings, potential_expressions)

                  if (generateScala.value.isDefined)
                    saveScala(potential_expressions, hashes)

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    logger.warn(t.toString)
                }
              //})

          } catch {
            case t: Throwable =>
              logger.warn(t.toString)
          }
        }
      //})
    } catch {
      case io: IOException =>
        logger.error(io.toString)
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

  def saveScala(expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))], hashes: Seq[Option[String]]): Unit = {
    val filename = lambdaFilename
    val file = scala.tools.nsc.io.File(filename)

    (expressions, hashes).zipped.foreach((f, hash) => {

      try {
        val stringRep = "{ " + Utils.dumpLambdaToString(f._1).replace("\n", "; ") + "}"

        val sha256 = hash.get

        synchronized {
          file.appendAll("(\"" + sha256 + "\",  " + s"Array($stringRep)) ,\n")
        }
      } catch {
        case t: Throwable =>
          logger.warn(t.toString)
      }
    })
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString

  def readLambdaFromFile(filename: String) =
    Eval(readFromFile(filename))

  def createValueMap(lambda: Lambda, sizes: Seq[ArithExpr] = Seq()): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.getVarsInParams()

    val actualSizes: Seq[ArithExpr] =
      if (sizes.isEmpty) Seq.fill(vars.length)(settings.searchParameters.defaultSize)
      else sizes

    (vars, actualSizes).zipped.toMap
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

  private def computeValidNDRanges(expr: Lambda): Seq[(NDRange, NDRange)] = {
    var usedDimensions: Set[Int] = Set()
    Expr.visit(expr.body,
      {
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
    val nDRangeDim = usedDimensions.max + 1

    logger.debug(s"computing ${nDRangeDim}D NDRanges")

    // hardcoded highest power of two = 8192
    val pow2 = Seq.tabulate(14)(x => scala.math.pow(2,x).toInt)
    val localGlobalCombinations: Seq[(ArithExpr, ArithExpr)] = (for {
      local <- pow2
      global <- pow2
      if local <= global
    } yield (local, global)).map{ case (l,g) => (Cst(l), Cst(g))}

    nDRangeDim match {
      case 1 => for {
        x <- localGlobalCombinations
        if ExpressionFilter(x._1, x._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, 1, 1), NDRange(x._2, 1, 1))

      case 2 => for {
        x: (ArithExpr, ArithExpr) <- localGlobalCombinations
        y: (ArithExpr, ArithExpr) <- localGlobalCombinations
        if ExpressionFilter(x._1, y._1, x._2, y._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, y._1, 1), NDRange(x._2, y._2, 1))

      case 3 => for {
        x <- localGlobalCombinations
        y <- localGlobalCombinations
        z <- localGlobalCombinations
        if ExpressionFilter(x._1, y._1, z._1, x._2, y._2, z._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, y._1, z._1), NDRange(x._2, y._2, z._2))

      case _ => throw new RuntimeException("Could not pre-compute NDRanges for exploration")
    }
  }
}
