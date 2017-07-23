package exploration

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import opencl.executor._
import opencl.generator.NDRange
import org.clapper.argot._

import scala.collection
import scala.io.Source
import scala.util.Random
import scala.util.parsing.json
import lift.arithmetic.ArithExpr




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
    (s,_)=>
      //take the first 3 values, try to convert them to number and fill ones if there were less than 3 values
      val localSizes = s.split(',').take(3).map(x=>x.toInt).padTo(3,1)
      NDRange(localSizes(0),localSizes(1),localSizes(2))
  }

  private val globalSize = parser.option[NDRange](List("gs", "globalSize"), "n",
    "Comma separated local sizes"){
    (s,_)=>
      val localSizes = s.split(',').take(3).map(x=>x.toInt).padTo(3,1)
      NDRange(localSizes(0),localSizes(1),localSizes(2))
  }

  private val vars = parser.option[Seq[ArithExpr]](List("vars"), "vars",
    "Comma separated vars"){
    (s,_) =>
      //try to parse all comma seperated values as ArithExpr
      s.split(',').map(x=>ArithExpr.IntToCst(x.toInt))
  }

  private val generateKernel = parser.flag[Boolean](List("execute","executeKernel"),
    "Shall we execute the Kernel or generate the openCL code?"){
    (s,_)=>s
  }

  private val envConf = parser.option[Map[String,Any]](List("env"), "env",
    "the path of the environment conf."){
    (s,_) =>
      json.JSON.parseFull(readFromFile(s)).get.asInstanceOf[Map[String,Any]]
  }




  def main(args: Array[String]): Unit = {
    parser.parse(args)

    //Es sollte geprüft werden, ob die Anzahl der gelesenen vars zum Ausdruck passt
    //Es sollte geprüft werden, ob überhaupt vars übergeben wurden.

    //gleiches für LS/GS

    val inputArgument = input.value.get

    var lambdaPath = Paths.get(inputArgument).toAbsolutePath.toString
    val lambdaStr = readFromFile(lambdaPath)
    val lowLevelFactory = Eval.getMethod(lambdaStr)

    var tuningWerte = vars.value.getOrElse(Seq.empty[ArithExpr])
    //TODO die LowLevelFactory wird sehr wütend, wenn man ihr zu wenig Werte gibt.
    //     Fehler Abfangen und Nutzer mitteilen, das die Anzahl der Parameter nicht stimmt.
    val lambda = lowLevelFactory(tuningWerte)

    //randomData muss aus dem passenden JSON gelesen werden

    //stencil1D
    val randomData = Seq.fill(1024)(Random.nextFloat()).toArray


    /* for convolution2D
    val randomData = Seq.fill(1024)(Seq.fill(1024)(Random.nextFloat()).toArray).toArray
    val randomData2 = Seq.fill(1024)(Random.nextFloat()).toArray
    */

    if (generateKernel.value.getOrElse(false)) {
      println("generating Kernel")
      val lowLevelName = Paths.get(inputArgument).getFileName
      val highLevelName = Paths.get(inputArgument).toAbsolutePath.getParent.getParent.getParent.getFileName
      val outputPath = Paths.get(inputArgument).toAbsolutePath.getParent.getParent.getParent.getParent.getParent + "/bestKernel.cl"
      generateAndSaveKernel(lambda, lowLevelName.toString, highLevelName.toString, outputPath)
    } else {
      var time = Int.MaxValue
      val ls = localSize.value.getOrElse(NDRange(1, 1, 1))
      val gs = globalSize.value.getOrElse(NDRange(1, 1, 1))

      try {
        //initialize the Executor

        //Read platform and device for execution from config file at home/.lift/environment.json
        val jsonFile = envConf.value.getOrElse(
          json.JSON.parseFull(
            readFromFile(System.getProperty("user.home") + "/.lift/environment.json")
          ).get.asInstanceOf[Map[String,Any]]
        )

        val platform = jsonFile("OpenCL").asInstanceOf[Map[String, Any]]("Platform").asInstanceOf[String]
        val device = jsonFile("OpenCL").asInstanceOf[Map[String, Any]]("Device").asInstanceOf[String]

       // val platform = jsonFile.get.asInstanceOf[Map[String, Any]]("OpenCL").asInstanceOf[Map[String, Any]]("Platform").asInstanceOf[String]
       // val device = jsonFile.get.asInstanceOf[Map[String, Any]]("OpenCL").asInstanceOf[Map[String, Any]]("Device").asInstanceOf[String]

        println("starting on platform: " + platform + ", device: " + device)

        Executor.loadLibrary()
        Executor.init(platform.toInt, device.toInt)
        //start Execution

        //stencil:
        val (output: Array[Float], kernelTime) = Execute(ls, gs, (true, true))(lambda, randomData)

        // for convolution2D:
        //val (output: Array[Float], kernelTime) = Execute(ls, gs, (true, true))(lambda, randomData, randomData2)
        println("Kernel time: " + kernelTime)
        //output in microseconds
        time = (kernelTime * 1000000).toInt
      }
      //we don't want to catch exceptions but we want to always write the costfile!
      finally{
        val outputPath = System.getProperty("user.dir")

        //convert time from seconds to nanoseconds and write to atf costfile
        writeToFile(outputPath + "/costfile.txt", time.toString)
        val line = (List(time,gs.x,gs.y,gs.z,ls.x,ls.y,ls.z)++tuningWerte).map(_.toString)
        appendToCsv(outputPath+"/times.csv", line)
      }
    }
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString

  def appendToCsv(filePath: String, line:Seq[String]): Unit = {
    val fw = new FileWriter(new File(filePath), true)
    fw.write(line.mkString(",")+"\n")
    fw.close()
  }


  def writeToFile(filePath: String, s: String): Unit = {
    val file = new File(filePath)
    //file.mkdirs()
    val fw = new FileWriter(file, false)
    val pw = new PrintWriter(fw)
    try pw.write(s) finally pw.close()
  }

  def generateAndSaveKernel(lambda:Lambda, lowLevelHash:String, highLevelHash:String, outputPath:String) = {

    //evtl. lieber SaveOpenCL benutzen, aber das ist komplex zu initialisiern ^^
    //andererseits ist der viel zu aufgeblasen
    //val possibleExp = Seq(lambda, vars.value.get, (NDRange(1,1,1), NDRange(1,1,1)))
    //SaveOpenCL(Paths.get(inputArgument).toAbsolutePath.getParent.getParent.getParent.getParent.toString.stripSuffix("Lower"), lowLevelName.toString, highLevelName.toString, null, possibleExp)

    //aus SaveOpenCL geklaut:
    //kernel bauen
    val compiled = Compile(lambda, localSize.value.getOrElse(NDRange(1,1,1)), globalSize.value.getOrElse(NDRange(1,1,1)))
    val kernel =
      s"""
         |// Substitutions: ${vars.value.get}
         |// Local sizes: ${localSize.value.getOrElse(NDRange(1,1,1))}
         |// Global sizes: ${globalSize.value.getOrElse(NDRange(1,1,1))}
         |// High-level hash: $highLevelHash
         |// Low-level hash: $lowLevelHash
         |
         |$compiled
         |""".stripMargin

    //kernel Speichern
    writeToFile(outputPath, kernel)
  }

}
