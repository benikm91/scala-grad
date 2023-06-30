package scalagrad.auto.forward.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.auto.forward.breeze.BreezeDualNumberDerivativeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.auto.breeze.BreezeMatrixAlgebra

object BreezeDualNumberMatrixAlgebra extends DualMatrixAlgebra[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
    DualNumberScalar[Double], 
    DualNumberColumnVector[DenseVector[Double]], 
    DualNumberRowVector[Transpose[DenseVector[Double]]], 
    DualNumberMatrix[DenseMatrix[Double]],
](
    BreezeMatrixAlgebra,
    BreezeDualNumberDerivativeMatrixAlgebra
)
