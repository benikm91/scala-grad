package scalagrad.api.forward

import scalagrad.api.DeriverPlan
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.ZeroOps
import scalagrad.api.matrixalgebra.derivative.OneOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scala.reflect.ClassTag

abstract class DeriverForwardPlan[
    // TODO currently type bounds are needed to make the compiler happy, but they should not be needed
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
](
    primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
) extends DeriverPlan[
    PScalar, PColumnVector, PRowVector, PMatrix,
    PScalar, PColumnVector, PRowVector, PMatrix,
    DualNumberScalar[PScalar], 
    DualNumberColumnVector[PColumnVector], 
    DualNumberRowVector[PRowVector], 
    DualNumberMatrix[PMatrix],
    DualNumberDerivativeMatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
](
    primaryMatrixAlgebra,
    DualNumberDerivativeMatrixAlgebra(primaryMatrixAlgebra)
):
    type DualScalar = DualNumberScalar[PScalar]
    type DualColumnVector = DualNumberColumnVector[PColumnVector]
    type DualRowVector = DualNumberRowVector[PRowVector]
    type DualMatrix = DualNumberMatrix[PMatrix]

     // FIXME
     // PScalar, PColumnVector, PRowVector, PMatrix should not have breeze type here, but I get a 
     // "the type test for DeriverForwardPlan.this.PMatrix cannot be checked at runtime because it refers to an abstract type member or type parameter"
     // don't know how to fix it.

    type OneOpsT = OneOps[PScalar, PColumnVector, PRowVector, PMatrix]
    val oneOps: OneOpsT

    import primaryMatrixAlgebra.*
    import derivativeMatrixAlgebra.*

    given scalar2Scalar: DeriverFromTo[DualScalar => DualScalar, PScalar => PScalar] with
        override def derive(f: DualScalar => DualScalar): PScalar => PScalar = x => 
            f(createDualScalar(x, oneOps.oneHotScalar)).dv

    given columnVector2Scalar: DeriverFromTo[DualColumnVector => DualScalar, PColumnVector => PColumnVector] with
        override def derive(f: DualColumnVector => DualScalar): PColumnVector => PColumnVector = x => 
            val res = for (i <- 0 until x.length)
                yield {
                    val dual = createDualColumnVector(x, oneOps.oneHotColumnVector(x.length, i))
                    f(dual).dv
                }
            createColumnVectorFromElements(res)

    given rowVector2Scalar: DeriverFromTo[DualRowVector => DualScalar, PRowVector => PRowVector] with
        override def derive(f: DualRowVector => DualScalar): PRowVector => PRowVector = x => 
            val res = for (i <- 0 until x.length)
                yield {
                    val dual = createDualRowVector(x, oneOps.oneHotRowVector(x.length, i))
                    f(dual).dv
                }
            createRowVectorFromElements(res)

    given matrix2Scalar: DeriverFromTo[DualMatrix => DualScalar, PMatrix => PMatrix] with
        override def derive(f: DualMatrix => DualScalar): PMatrix => PMatrix = x => 
            val res = for (i <- 0 until x.nRows * x.nCols)
                yield {
                    val dual = createDualMatrix(x, oneOps.oneHotMatrix(x.nRows, x.nCols, i))
                    f(dual).dv
                }
            createMatrixFromElements(x.nRows, x.nCols, res)

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[T => DualScalar, DualTupleToPTuple[T] => DualTupleToPTuple[T]] with

        private val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

        override def derive(f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T] = t => 
            def toZeros(t: DualTupleToPTuple[T]): T = 
                t.map[[X] =>> PToDual[X]]([T] => (t: T) => t match {
                    case x: PScalar => createDualScalar(x, dZeroOps.zeroScalar)
                    case x: PColumnVector => createDualColumnVector(x, dZeroOps.zeroColumnVector(x.length))
                    case x: PRowVector => createDualRowVector(x, dZeroOps.zeroRowVector(x.length))
                    case x: PMatrix => createDualMatrix(x, dZeroOps.zeroMatrix(x.nRows, x.nCols))
                }).asInstanceOf[T]
            def forwardPlan(zeros: T): DualTupleToPTuple[T] = 
                val zerosWithIndex = zeros.zip(indices)
                def appendTopAndBottomTo(top: Tuple, x: Any, bottom: Tuple) = 
                    top ++ (x *: bottom)
                zerosWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (x: DualNumberScalar[PScalar], y: Int) => 
                        val (top, bottom) = zeros.splitAt(y)
                        val dual = createDualScalar(x.v, oneOps.oneHotScalar)
                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                    case (x: DualColumnVector, y: Int) => 
                        val (top, bottom) = zeros.splitAt(y)
                        val res = for (i <- 0 until x.v.length)
                            yield {
                                val dual = createDualColumnVector(x.v, oneOps.oneHotColumnVector(x.v.length, i))
                                f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                            }
                        createColumnVectorFromElements(res)
                    case (x: DualRowVector, y: Int) => 
                        val (top, bottom) = zeros.splitAt(y)
                        val res = for (i <- 0 until x.v.length)
                            yield {
                                val dual = createDualRowVector(x.v, oneOps.oneHotRowVector(x.v.length, i))
                                f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                            }
                        createRowVectorFromElements(res)
                    case (x: DualMatrix, y: Int) =>
                        val (top, bottom) = zeros.splitAt(y)
                        val res = for (i <- 0 until x.v.nRows * x.v.nCols)
                            yield {
                                val dual = createDualMatrix(x.v, oneOps.oneHotMatrix(x.v.nRows, x.v.nCols, i))
                                f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                            }
                        createMatrixFromElements(x.v.nRows, x.v.nCols, res)
                }).asInstanceOf[DualTupleToPTuple[T]]
            forwardPlan(toZeros(t))
