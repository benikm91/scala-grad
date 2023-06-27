package scalagrad.auto.forward.breeze

import scalagrad.api.forward.DeriverForwardPlan
import breeze.linalg.*
import scalagrad.api.Deriver
import scalagrad.api.forward.dual.DualNumberMatrix
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.forward.dual.DualNumberRowVector
import scalagrad.api.forward.dual.DualNumberColumnVector
import scalagrad.api.matrixalgebra.CreateOps
import scala.reflect.ClassTag
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra

object DeriverBreezeForwardPlan extends DeriverForwardPlan[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](
    BreezeMatrixAlgebra
):

    type PScalar = Double
    type PColumnVector = DenseVector[Double]
    type PRowVector = Transpose[DenseVector[Double]]
    type PMatrix = DenseMatrix[Double]

    override val oneOps: OneOpsT = new OneOpsT:
        override def oneHotScalar: PScalar = 1.0
        override def oneHotColumnVector(length: Int, i: Int): PColumnVector = 
            val res = DenseVector.zeros[PScalar](length)
            res(i) = 1.0
            res
        override def oneHotRowVector(length: Int, i: Int): PRowVector = oneHotColumnVector(length, i).t
        override def oneHotMatrix(nRows: Int, nCols: Int, i: Int): PMatrix = 
            val res = DenseMatrix.zeros[PScalar](nRows, nCols)
            res(i % nRows, i / nRows) = 1.0
            res
