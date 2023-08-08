package scalagrad.api

import scalagrad.api.dual.DualMatrixAlgebraT
import breeze.linalg.{DenseVector, Transpose, DenseMatrix}

type DualBreezeMatrixAlgebraT = DualMatrixAlgebraT {
    type PrimaryScalar = Double
    type PrimaryColumnVector = DenseVector[Double]
    type PrimaryRowVector = Transpose[DenseVector[Double]]
    type PrimaryMatrix = DenseMatrix[Double]
}

object DualBreezeMatrixAlgebraT:

    extension(algebraT: DualBreezeMatrixAlgebraT)
        def lift(xs: DenseMatrix[Double]): algebraT.Matrix = 
            val algebra = algebraT.innerAlgebra
            algebra.derivativeMatrixAlgebra.createDualMatrix(
                xs,
                algebra.derivativeMatrixAlgebra.dZeroOps.zeroMatrix(xs.rows, xs.cols),
                List(),
            )
        def lift(xs: DenseVector[Double]): algebraT.ColumnVector = 
            val algebra = algebraT.innerAlgebra
            algebra.derivativeMatrixAlgebra.createDualColumnVector(
                xs,
                algebra.derivativeMatrixAlgebra.dZeroOps.zeroColumnVector(xs.length),
                List(),
            )
        def lift(xs: Transpose[DenseVector[Double]]): algebraT.RowVector = 
            val algebra = algebraT.innerAlgebra
            algebra.derivativeMatrixAlgebra.createDualRowVector(
                xs,
                algebra.derivativeMatrixAlgebra.dZeroOps.zeroRowVector(xs.inner.length),
                List(),
            )
        def lift(xs: Double): algebraT.Scalar = 
            val algebra = algebraT.innerAlgebra
            algebra.derivativeMatrixAlgebra.createDualScalar(
                xs,
                algebra.derivativeMatrixAlgebra.dZeroOps.zeroScalar,
                List(),
            )