package scalagrad.auto.forward.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{AccessOps, CreateOps, LiftOps, NegateOps, ScalarInvertOps, SumOps, TransposeOps}
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.forward.DualNumberDerivativeMatrixAlgebra
import scalagrad.auto.breeze.BreezeMatrixAlgebra

object BreezeDualNumberDerivativeMatrixAlgebra extends DualNumberDerivativeMatrixAlgebra[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double]
](BreezeMatrixAlgebra)
