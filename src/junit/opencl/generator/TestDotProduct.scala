package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

object TestDotProduct {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestDotProduct {

  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val abs = UserFunDef("abs", "x", "{ return x >= 0 ? x : -x; }", Float, Float)

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x+y; }", TupleType(Float, Float), Float)

  val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", TupleType(Float, Float), Float)

  val mult = UserFunDef("mult", Array("l", "r"), "{ return l * r; }", TupleType(Float, Float), Float)

  val multAndSumUp = UserFunDef("multAndSumUp", Array("acc", Array("l", "r")),
    "{ return acc + (l * r); }",
    TupleType(Float, TupleType(Float, Float)), Float)

  val multAndSumUp3 = UserFunDef("multAndSumUp3", Array("acc", "l", "r"),
    "{ return acc + (l * r); }",
    TupleType(Float, Float, Float), Float)

  val N = Var("N")
  val M = Var("M")

  private def dotProd(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).reduce(_+_)
  }

  @Test def DOT_PRODUCT_SIMPLE() {

    val inputSize = 1024
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputSize)(fun (ArrayType(Float, Var("N")),
                                                    ArrayType(Float, Var("N")), (left, right) => {

      Join() o MapWrg(
        fun( (x) => Join() o MapLcl(fun( (x) => ReduceSeq(sumUp, 0.0f) o MapSeq(mult) o x)) o Split(4) o x )
      ) o Split(1024) o Zip(left, right)

    }), leftInputData, rightInputData, leftInputData.size, rightInputData.size )

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(dotProd(leftInputData, rightInputData), output.reduce(_ + _), 0.0)
  }

  @Test def DOT_PRODUCT_CPU() {

    val inputSize = 262144
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputSize)( fun (ArrayType(Float, Var("N")),
                                                       ArrayType(Float, Var("N")),(left, right) => {

        Join() o Join() o MapWrg(
          toGlobal(MapLcl(ReduceSeq(multAndSumUp, 0.0f)))
        ) o Split(128) o Split(2048) o Zip(left, right)

      }), leftInputData, rightInputData, leftInputData.size, rightInputData.size )

      println("output.size = " + output.size)
      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = opencl.executor.Execute(firstOutput.length)( fun (ArrayType(Float, Var("N")),(in) => {

        Join() o MapWrg(
          Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(128)
        ) o Split(128) o in

      }), firstOutput, firstOutput.length )

      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.reduce(_ + _), 0.0)

      (output, runtime)
    }
  }

  @Test def DOT_PRODUCT() {

    val inputSize = 262144
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = opencl.executor.Execute(inputSize)( fun(ArrayType(Float, Var("N")),
                                                                      ArrayType(Float, Var("N")), (left, right) => {

        Join() o Join() o MapWrg(
          toGlobal(MapLcl(ReduceSeq(multAndSumUp, 0.0f))) o ReorderStride()
        ) o Split(128) o Split(2048) o Zip(left, right)

      }), leftInputData, rightInputData, leftInputData.length, rightInputData.length )

      println("output.size = " + output.size)
      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = opencl.executor.Execute(firstOutput.length)( fun (ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(6)(Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2)) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2)
        ) o Split(128) o in

      }), firstOutput, firstOutput.length )

      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

  }

}