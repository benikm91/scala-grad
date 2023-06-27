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
        BreezeTestUtil.reasonableDenseMatrixDoubleGenerator,
        BreezeTestUtil.reasonableDenseVectorDoubleGenerator,
        (x: Gen[Int]) => for { cv <- BreezeTestUtil.reasonableDenseVectorDoubleGenerator(x) } yield cv.t,
        BreezeTestUtil.reasonableDoubleGenerator,
        DeriverBreezeNumericalPlan,
      )

    val (min, max) = (1e-3, 1e+1)
    def reasonableDoubleGenerator: Gen[Double] = 
      Gen.oneOf(
        Gen.choose(min, max),
        Gen.choose(-max, -min)
      )

    def isReasonableDouble(x: Double): Boolean = 
      def isInRange(x: Double): Boolean = min <= x && x <= max
      isInRange(x) || isInRange(-x)

    def reasonableDenseMatrixDoubleGenerator: (Gen[Int], Gen[Int]) => Gen[DenseMatrix[Double]] = reasonableDenseMatrixDoubleGenerator(reasonableDoubleGenerator)
    def reasonableDenseMatrixDoubleGenerator(doubleGenerator: Gen[Double]): (Gen[Int], Gen[Int]) => Gen[DenseMatrix[Double]] =
      (nRowsGen: Gen[Int], nColsGen: Gen[Int]) =>
        for
          nRows <- nRowsGen
          nCols <- nColsGen
          xs <- Gen.listOfN(nRows * nCols, doubleGenerator)
        yield new DenseMatrix(nRows, nCols, xs.toArray)

    def isReasonableDenseMatrixDouble(x: DenseMatrix[Double]): Boolean = 
        x.forall(isReasonableDouble)

    def reasonableDenseVectorDoubleGenerator: Gen[Int] => Gen[DenseVector[Double]] = reasonableDenseVectorDoubleGenerator(reasonableDoubleGenerator)
    def reasonableDenseVectorDoubleGenerator(doubleGenerator: Gen[Double]): Gen[Int] => Gen[DenseVector[Double]] = 
      lengthGen =>
        for
          length <- lengthGen
          xs <- Gen.listOfN(length, doubleGenerator)
        yield DenseVector(xs.toArray)

    def isReasonableDenseVectorDouble(x: DenseVector[Double]): Boolean = 
        x.forall(isReasonableDouble)