package opencl.generator

import arithmetic.Var
import ir.UserFunDef._
import ir.{Split, Join, ArrayType, fun}
import opencl.executor.{Executor, Compile, Execute}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestAddressSpaces {
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

class TestAddressSpaces {
  @Test def localGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def staticLocalMemory(): Unit = {
    val orig = AllocateStatically()
    AllocateStatically(allocateStatically = true)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val code = Compile(f)

    val (output, runtime) = Execute(inputSize)(code, f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, OpenCLGenerator.Kernel.memory.length)

    AllocateStatically(orig)
  }

  @Test def nonStaticLocalMemory(): Unit = {
    val orig = AllocateStatically()
    AllocateStatically(allocateStatically = false)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val code = Compile(f)

    val (output, runtime) = Execute(inputSize)(code, f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(3, OpenCLGenerator.Kernel.memory.length)

    AllocateStatically(orig)
  }

}
