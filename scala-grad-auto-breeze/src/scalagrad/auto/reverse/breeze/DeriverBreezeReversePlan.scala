package scalagrad.auto.reverse.breeze

import scalagrad.api.forward.DeriverForwardPlan
import breeze.linalg.*
import scalagrad.api.Deriver
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.reverse.delta.*
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.DeriverReversePlan

object DeriverBreezeReversePlan extends DeriverReversePlan[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](
    BreezeMatrixAlgebra
):

    type PScalar = Double
    type PColumnVector = DenseVector[Double]
    type PRowVector = Transpose[DenseVector[Double]]
    type PMatrix = DenseMatrix[Double]

    type DScalar = DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
    type DColumnVector = DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DRowVector = DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DMatrix = DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

    type BreezeDualScalar = DualDeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
    type BreezeDualColumnVector = DualDeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type BreezeDualRowVector = DualDeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type BreezeDualMatrix = DualDeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] 
