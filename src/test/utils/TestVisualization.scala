package utils;

import c.executor.Compile
import ir.{ArrayType, ArrayTypeWSWC, TypeChecker}
import ir.ast.{PrintType, Split, _}
import lift.arithmetic.{SizeVar, Var}
import opencl.executor.{Execute, Executor, TestWithExecutor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

object TestVisualization extends TestWithExecutor

class TestVisualization{

  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}

    @Test
    def psiPaperTestExpression(): Unit = {
        def M = Var("M")
        def N = Var("N")
      val expressionText = " PrintType(visual = true,render = true) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)"
        def expression =  PrintType(visual = true,render = true,expressionText) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)

                val lambda = \(ArrayType(Float, N), input => expression $ input)
        TypeChecker(lambda)
    }

  @Test
  def using3DArrays(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")
    val O = SizeVar("O")
    val expressionText = "MapGlb(id) o PrintType(visual = true,render = true) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true)"
    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, N), M ), O),
      input => MapGlb(id) o PrintType(visual = true,render = true,expressionText) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true) $ input
    )
    TypeChecker(lambda)
    //println(Compile(lambda))
  }


  @Test
  def tupleType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText =" MapGlb(\\(tuple => id(tuple._0))) o PrintType(visual = true,render = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        MapGlb(\(tuple => id(tuple._0))) o PrintType(visual = true,render = true,expressionText) $ Zip(input, input)
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test
  def vectorType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText ="PrintType(visual = true,render = true) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        PrintType(visual = true,render = true,expressionText) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)  $ input
    )

    TypeChecker(lambda)
   // val kernel = Compile(lambda)
    //println(kernel)
  }

  @Test def MATRIX_MATRIX_SIMPLER(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val Msize = 64
    val Ksize = 128
    val Nsize = 256
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val expression = "(A, B) => {\n        PrintType(true,true) o MapGlb(fun( Arow =>\n          PrintType(true) o MapSeq(fun( Bcol =>\n            PrintType(true) o toGlobal( MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) o PrintType(true) $ Zip(Arow, Bcol)\n          )) o PrintType(true) $ B\n        )) o PrintType(true) $ A\n      })"
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        PrintType(true,true,expression) o MapGlb(fun( Arow =>
          PrintType(true) o MapSeq(fun( Bcol =>
            PrintType(true) o toGlobal( MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) o PrintType(true) $ Zip(Arow, Bcol)
          )) o PrintType(true) $ B
        )) o PrintType(true) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB.transpose)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }





  @Test def tiledMatrixMultiply(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

     val N = SizeVar("N")
     val M = SizeVar("M")
     val K = SizeVar("K")

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten
    val expression = "      (A, B) => {\n        // Undo the tiling\n        PrintType(true,true,expression) o Untile2D() o\n          PrintType(true) o MapWrg(0)(fun( aRows =>\n            PrintType(true) o MapWrg(1)(fun( bCols =>\n\n              // Reduce the partial results (matrices), so that the reduce is innermost\n              PrintType(true) o MapLcl(0)(PrintType(true) o Join() o PrintType(true) o  MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()) o PrintType(true) o  Transpose())  o Transpose()  o PrintType(true) o\n\n                // Multiply all necessary combinations of tiles\n                 PrintType(true) o MapSeq(fun( tiles =>\n                  MapLcl(0)( fun(aTile =>\n                    MapLcl(1)( fun( bTile =>\n                         toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(aTile, bTile)\n                    )) $ Get(tiles, 1)\n                  )) $ Get(tiles, 0)\n                )) o PrintType(true) $ Zip(aRows, bCols)\n\n              // Tile the matrices\n            )) o PrintType(true) o Tile(tileSize)o PrintType(true) $ B\n          )) o Tile(tileSize) $ A"
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        PrintType() o Untile2D() o
          PrintType() o MapWrg(0)(fun( aRows =>
            PrintType() o MapWrg(1)(fun( bCols =>

              // Reduce the partial results (matrices), so that the reduce is innermost
              PrintType() o MapLcl(0)(PrintType() o Join() o PrintType() o  MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()) o PrintType() o  Transpose())  o Transpose()  o PrintType() o

                // Multiply all necessary combinations of tiles
                 PrintType() o MapSeq(fun( tiles =>
                  MapLcl(0)( fun(aTile =>
                    MapLcl(1)( fun( bTile =>
                         toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(aTile, bTile)
                    )) $ Get(tiles, 1)
                  )) $ Get(tiles, 0)
                )) o PrintType() $ Zip(aRows, bCols)

              // Tile the matrices
            )) o PrintType() o Tile(tileSize)o PrintType() $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (false, false))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

}


