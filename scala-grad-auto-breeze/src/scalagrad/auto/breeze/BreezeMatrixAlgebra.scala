package scalagrad.auto.breeze

import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scalagrad.api.forward.dual.*

object BreezeMatrixAlgebra extends MatrixAlgebra[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
]:

  override def one: ScalarT = 1.0

  override def inverse(m: MatrixT): MatrixT = ???

  override def determinant(m: MatrixT): ScalarT = ???

  override def elementAtM(m: MatrixT, iRow: Int, jColumn: Int): ScalarT = m(iRow, jColumn)

  override def columnAtM(m: MatrixT, jColumn: Int): ColumnVectorT = m(::, jColumn)

  override def elementAtCV(cv: ColumnVectorT, iRow: Int): ScalarT = cv(iRow)

  override def setElementAtM(m: MatrixT, iRow: Int, jColumn: Int, newValue: ScalarT): MatrixT =
    val newM = m.copy
    newM(iRow, jColumn) = newValue
    newM

  override def setColumnAtM(m: MatrixT, jColumn: Int, newValue: ColumnVectorT): MatrixT =
    val newM = m.copy
    newM(::, jColumn) := newValue
    newM

  override def setElementAtCV(cv: ColumnVectorT, i: Int, newValue: ScalarT): ColumnVectorT = 
    val newCv = cv.copy
    newCv(i) = newValue
    newCv

  override def dotMM(m1: MatrixT, m2: MatrixT): MatrixT = m1 * m2

  override def dotMCV(m: MatrixT, cv: ColumnVectorT): ColumnVectorT = m * cv

  override def dotCVRV(cv: ColumnVectorT, rv: RowVectorT): MatrixT = cv * rv

  override def dotRVCV(rv: RowVectorT, cv: ColumnVectorT): ScalarT = rv * cv

  override def multiplyMS(m: MatrixT, s: ScalarT): MatrixT = m * s
  
  override def multiplyCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = cv * s

  override def multiplySS(s1: ScalarT, s2: ScalarT): ScalarT = s1 * s2

  override def plusMM(m1: MatrixT, m2: MatrixT): MatrixT = m1 + m2

  override def plusMCV(m: MatrixT, cv: ColumnVectorT): MatrixT = m(::, breeze.linalg.*) + cv

  override def plusMS(m: MatrixT, s: ScalarT): MatrixT = m + s

  override def plusCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT = cv1 + cv2

  override def plusCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = cv + s

  override def plusSS(s1: ScalarT, s2: ScalarT): ScalarT = s1 + s2

  override def elementWiseMultiplyMM(m1: MatrixT, m2: MatrixT): MatrixT = m1 *:* m2

  override def elementWiseMultiplyCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT = cv1 *:* cv2

  override def numberOfRows(m: MatrixT): Int = m.rows

  override def numberOfCols(m: MatrixT): Int = m.cols

  override def lengthColumnVector(cv: ColumnVectorT): Int = cv.length

  override def negateS(s: ScalarT): ScalarT = -s

  override def negateCV(cv: ColumnVectorT): ColumnVectorT = -cv

  override def negateM(m: MatrixT): MatrixT = -m

  override def invert(s: ScalarT): ScalarT = 1.0 / s

  override def unliftToDouble(s: ScalarT): Double = s

  override def liftToScalar(d: Double): ScalarT = d

  override def sumCV(cv: ColumnVectorT): ScalarT = breeze.linalg.sum(cv)

  override def sumM(m: MatrixT): ScalarT = breeze.linalg.sum(m)

  override def createMatrixFromElements(nRows: Int, nCols: Int, elements: Seq[ScalarT]): MatrixT = 
      new DenseMatrix(nRows, nCols, elements.toArray)

  override def stackColumns(columns: Seq[ColumnVectorT]): MatrixT = DenseMatrix.horzcat(columns.map(_.toDenseMatrix):_*)

  override def createColumnVectorFromElements(elements: Seq[ScalarT]): ColumnVectorT = DenseVector(elements.toArray)

  override def transpose(m: MatrixT): MatrixT = m.t

  override def transposeColumVector(cv: ColumnVectorT): RowVectorT = cv.t

  override def transposeRowVector(v: RowVectorT): ColumnVectorT = v.t
  
  override def zeroScalar: Double = 0.0

  override def zeroColumnVector(length: Int): DenseVector[Double] = DenseVector.zeros[Double](length)
    
  override def zeroRowVector(length: Int): Transpose[DenseVector[Double]] = DenseVector.zeros[Double](length).t

  override def zeroMatrix(nRows: Int, nCols: Int): DenseMatrix[Double] = DenseMatrix.zeros[Double](nRows, nCols)