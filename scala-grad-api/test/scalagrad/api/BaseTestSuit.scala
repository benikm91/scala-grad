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
import scalagrad.api.Mode

trait BaseTestSuit extends should.Matchers with BasicGenerators:
    
    val deriver: Mode
    val pma: MatrixAlgebraDSL
    val testName: String

    val tolerance: Double = 0.5

    def compareAllElementsMS(t1: (pma.Matrix, pma.Scalar), t2: (pma.Matrix, pma.Scalar)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsMRV(t1: (pma.Matrix, pma.RowVector), t2: (pma.Matrix, pma.RowVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsRVRV(t1._2, t2._2)

    def compareAllElementsMCV(t1: (pma.Matrix, pma.ColumnVector), t2: (pma.Matrix, pma.ColumnVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsMM(t1: (pma.Matrix, pma.Matrix), t2: (pma.Matrix, pma.Matrix)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsMM(t1._2, t2._2)

    def compareAllElementsCVCV(t1: (pma.ColumnVector, pma.ColumnVector), t2: (pma.ColumnVector, pma.ColumnVector)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsCVS(t1: (pma.ColumnVector, pma.Scalar), t2: (pma.ColumnVector, pma.Scalar)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsSS(t1: (pma.Scalar, pma.Scalar), t2: (pma.Scalar, pma.Scalar)) = 
        compareElementsSS(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareElementsSS(s1: pma.Scalar, s2: pma.Scalar) = 
        s1.toDouble should be(s2.toDouble +- tolerance)

    def compareElementsCVCV(cv1: pma.ColumnVector, cv2: pma.ColumnVector) =
        cv1.length should be(cv2.length)
        cv1.elements.zip(cv2.elements).foreach { (e1, e2) =>
            e1.toDouble should be(e2.toDouble +- tolerance)
        }

    def compareElementsRVRV(rv1: pma.RowVector, rv2: pma.RowVector) = compareElementsCVCV(
        pma.transposeRowVector(rv1), 
        pma.transposeRowVector(rv2),
    )

    def compareElementsMM(m1: pma.Matrix, m2: pma.Matrix) = 
        m1.nRows should be(m2.nRows)
        m1.nCols should be(m2.nCols)
        m1.elements.zip(m2.elements).foreach { (e1, e2) =>
            e1.toDouble should be(e2.toDouble +- tolerance)
        }

    