package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.DeriverPlan
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.dual
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import scalagrad.numerical.DeriverNumericalPlan
import scalagrad.api.forward.DeriverForwardPlan
import scalagrad.api.reverse.DeriverReversePlan
import breeze.linalg.*
import scalagrad.api.matrixalgebra.MatrixAlgebra

case class GlobalTestSuitParams[
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar],
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector],
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector],
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
](
    testName: String,
    dualAlgebra: MatrixAlgebra[DualScalar, DualColumnVector, DualRowVector, DualMatrix],
    primaryAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
    mGen: (Gen[Int], Gen[Int]) => Gen[PMatrix],
    cvGen: (Gen[Int]) => Gen[PColumnVector],
    rvGen: (Gen[Int]) => Gen[PRowVector],
    sGen: Gen[PScalar],
    deriverNumericalPlan: DeriverNumericalPlan[PScalar, PColumnVector, PRowVector, PMatrix],
)