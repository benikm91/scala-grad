package scalagrad.auto.reverse.breeze

import scalagrad.api.forward.DeriverForwardPlan
import breeze.linalg.*
import scalagrad.api.Deriver
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.reverse.delta.*
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.DeriverReversePlan

object DeriverBreezeDoubleReversePlan extends DeriverReversePlan[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](
    BreezeDoubleMatrixAlgebra
)