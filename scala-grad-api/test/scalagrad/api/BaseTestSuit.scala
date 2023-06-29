package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.DeriverPlan
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
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import scalagrad.numerical.DeriverNumericalPlan
import scalagrad.api.forward.DeriverForwardPlan
import scalagrad.api.reverse.DeriverReversePlan

import breeze.linalg.*
import scalagrad.api.matrixalgebra.MatrixAlgebra

trait BaseTestSuit[
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
](
) extends should.Matchers:
    
    val primaryAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix]
    val tolerance: Double = 1

    def compareAllElementsMS(t1: (PMatrix, PScalar), t2: (PMatrix, PScalar)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsMRV(t1: (PMatrix, PRowVector), t2: (PMatrix, PRowVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsRVRV(t1._2, t2._2)

    def compareAllElementsMCV(t1: (PMatrix, PColumnVector), t2: (PMatrix, PColumnVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsMM(t1: (PMatrix, PMatrix), t2: (PMatrix, PMatrix)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsMM(t1._2, t2._2)

    def compareAllElementsCVCV(t1: (PColumnVector, PColumnVector), t2: (PColumnVector, PColumnVector)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsCVS(t1: (PColumnVector, PScalar), t2: (PColumnVector, PScalar)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsSS(t1: (PScalar, PScalar), t2: (PScalar, PScalar)) = 
        compareElementsSS(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareElementsSS(s1: PScalar, s2: PScalar) = 
        (s1: Double) should be((s2: Double) +- tolerance)

    def compareElementsCVCV(cv1: PColumnVector, cv2: PColumnVector) =
        cv1.length should be(cv2.length)
        cv1.toArray.zip(cv2.toArray).foreach { (e1, e2) =>
            e1 should be(e2 +- tolerance)
        }

    def compareElementsRVRV(rv1: PRowVector, rv2: PRowVector) = compareElementsCVCV(
        primaryAlgebra.transposeRowVector(rv1), 
        primaryAlgebra.transposeRowVector(rv2),
    )

    def compareElementsMM(m1: PMatrix, m2: PMatrix) = 
        m1.rows should be(m2.rows)
        m1.cols should be(m2.cols)
        m1.toArray.zip(m2.toArray).foreach { (e1, e2) =>
            e1 should be(e2 +- tolerance)
        }
