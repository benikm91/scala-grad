package scalagrad.util.test

import org.scalacheck.Gen
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

object BreezeTestUtil:

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