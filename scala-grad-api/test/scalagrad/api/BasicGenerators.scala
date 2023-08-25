package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.Mode
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.dual

import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen

import scalagrad.api.matrixalgebra.MatrixAlgebraDSL

trait BasicGenerators:
    this: BaseTestSuit =>

    import BreezeTestUtil.*

    def mGen: (Gen[Int], Gen[Int]) => Gen[pma.Matrix] = (nRows: Gen[Int], nCols: Gen[Int]) => denseMatrixDoubleGenerator(reasonableDoubleGenerator)(nRows, nCols).map(pma.lift)
    def positiveOnlyMGen: (Gen[Int], Gen[Int]) => Gen[pma.Matrix] = (nRows: Gen[Int], nCols: Gen[Int]) => denseMatrixDoubleGenerator(reasonablePositiveDoubleGenerator)(nRows, nCols).map(pma.lift)
    def cvGen: (Gen[Int]) => Gen[pma.ColumnVector] = (length: Gen[Int]) => denseVectorDoubleGenerator(reasonableDoubleGenerator)(length).map(pma.lift)
    def rvGen: (Gen[Int]) => Gen[pma.RowVector] = (x: Gen[Int]) => (for { cv <- denseVectorDoubleGenerator(reasonableDoubleGenerator)(x) } yield cv.t).map(pma.lift)
    def sGen: Gen[pma.Scalar] = reasonableDoubleGenerator.map(pma.lift)
    def smallSGen: Gen[pma.Scalar] = reasonableSmallDoubleGenerator.map(pma.lift)
    def positiveOnlySGen: Gen[pma.Scalar] = reasonablePositiveDoubleGenerator.map(pma.lift)


import org.scalacheck.Gen
import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.dual
import scalagrad.api.matrixalgebra.MatrixAlgebra

object BreezeTestUtil:

    private val (min, max) = (1e-3, 1e+3)
    
    def reasonableSmallDoubleGenerator: Gen[Double] = 
      Gen.oneOf(
        Gen.choose(1e-3, 1e-2),
        Gen.choose(-1e-2, -1e-3)
      )

    def reasonableDoubleGenerator: Gen[Double] = 
      Gen.oneOf(
        Gen.choose(min, max),
        Gen.choose(-max, -min)
      )

    def reasonablePositiveDoubleGenerator: Gen[Double] = Gen.choose(min, max)

    def isReasonableDouble(x: Double): Boolean = 
      def isInRange(x: Double): Boolean = min <= x && x <= max
      isInRange(x) || isInRange(-x)

    def denseMatrixDoubleGenerator(valueGenerator: Gen[Double])(nRowsGen: Gen[Int], nColsGen: Gen[Int]): Gen[DenseMatrix[Double]] =
      for
        nRows <- nRowsGen
        nCols <- nColsGen
        xs <- Gen.listOfN(nRows * nCols, valueGenerator)
      yield new DenseMatrix(nRows, nCols, xs.toArray)

    def isReasonableDenseMatrixDouble(x: DenseMatrix[Double]): Boolean = 
        x.forall(isReasonableDouble)

    def denseVectorDoubleGenerator(valueGenerator: Gen[Double])(lengthGen: Gen[Int]): Gen[DenseVector[Double]] = 
      for
        length <- lengthGen
        xs <- Gen.listOfN(length, valueGenerator)
      yield DenseVector(xs.toArray)

    def isReasonableDenseVectorDouble(x: DenseVector[Double]): Boolean = 
        x.forall(isReasonableDouble)