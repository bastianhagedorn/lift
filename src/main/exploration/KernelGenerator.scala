package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.Logger
import exploration.KernelGenerator.{parser, readFromFile}
import exploration.ParameterSearch.SubstitutionMap
import ir.ArrayTypeWSWC
import ir.ast.{Join, Pad, Pad3D, Slide3D, \, fun}
import opencl.executor.{Eval, Execute}
import opencl.generator.NDRange
import org.clapper.argot._

import scala.io.Source
import scala.util.Random
import lift.arithmetic.{ArithExpr, Cst}
import opencl.ir.pattern._
import opencl.ir.{Float, add, id}

//vom StencilTest
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._




/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object KernelGenerator {

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



  def main(args: Array[String]): Unit = {


    parser.parse(args)

    val inputArgument = input.value.get

    //we need the relative path from topfolder to the lambda
    var lambdaPath = Paths.get(inputArgument).toAbsolutePath.toString
    val lambdaStr = readFromFile(lambdaPath)
    val lowLevelFactory = Eval.getMethod(lambdaStr)

    //Diese sollen eingelesen werden:
    val tuningWerte = Array[ArithExpr](1024, 32)
    val lambda = lowLevelFactory(tuningWerte)


    println("lambdaStr: " + lambdaStr)
    println("lambda: " + lambda)


    val randomData = Seq.fill(1024)(Random.nextFloat()).toArray


    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()

    //Hier müssen wir mit der Angabe der GS und LS noch schauen, dass wir die auch aus den eingaben holen
    val (output: Array[Float], time) = Execute(1,1,1,32,1,1,(true,true))(lambda, randomData)
    //nach def von Execute.Scala sollte das eig so gehen, klappt aber nicht...
    //val (output: Array[Float], time) = Execute(localSize, globalSize, (true, true))(lambda, randomData)

    println("time: " + time)
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString


}
