package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.Mode
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.dual
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import scalagrad.api.forward.ForwardMode
import scalagrad.api.reverse.ReverseMode
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scala.reflect.Typeable
import breeze.linalg.*

case class GlobalTestSuitParams[
    Scalar, ColumnVector, RowVector, Matrix
](
    testName: String,
    mGen: (Gen[Int], Gen[Int]) => Gen[Matrix],
    positiveOnlyMGen: (Gen[Int], Gen[Int]) => Gen[Matrix],
    cvGen: (Gen[Int]) => Gen[ColumnVector],
    rvGen: (Gen[Int]) => Gen[RowVector],
    sGen: Gen[Scalar],
    smallSGen: Gen[Scalar],
    positiveOnlySGen: Gen[Scalar]
)