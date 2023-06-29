package scalagrad.util.test

import org.scalacheck.Gen
import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.dual
import scalagrad.api.matrixalgebra.MatrixAlgebra

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.GlobalTestSuitParams

object BreezeTestUtil:

    def createGlobalTestSuitParams[
      DScalar, DColumnVector, DRowVector, DMatrix,
      DualScalar <: dual.DualScalar[Double, DScalar],
      DualColumnVector <: dual.DualColumnVector[DenseVector[Double], DColumnVector],
      DualRowVector <: dual.DualRowVector[Transpose[DenseVector[Double]], DRowVector],
      DualMatrix <: dual.DualMatrix[DenseMatrix[Double], DMatrix],
    ](
      testName: String,
      dualAlgebra: MatrixAlgebra[DualScalar, DualColumnVector, DualRowVector, DualMatrix],
    ): GlobalTestSuitParams[
      Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
      DScalar, DColumnVector, DRowVector, DMatrix,
      DualScalar, DualColumnVector, DualRowVector, DualMatrix,
    ] =
      GlobalTestSuitParams(
        testName,
        dualAlgebra,
        DeriverBreezeNumericalPlan.algebra,
        denseMatrixDoubleGenerator(reasonableDoubleGenerator),
        denseMatrixDoubleGenerator(reasonablePositiveDoubleGenerator),
        denseVectorDoubleGenerator(reasonableDoubleGenerator),
        (x: Gen[Int]) => for { cv <- denseVectorDoubleGenerator(reasonableDoubleGenerator)(x) } yield cv.t,
        reasonableDoubleGenerator,
        reasonableSmallDoubleGenerator,
        reasonablePositiveDoubleGenerator,
        DeriverBreezeNumericalPlan,
      )

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