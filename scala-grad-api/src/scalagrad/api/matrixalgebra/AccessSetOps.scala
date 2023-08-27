package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait AccessSetOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def setElementAtM(m: Matrix, iRow: Int, jColumn: Int, newValue: Scalar): Matrix
    def setColumnAtM(m: Matrix, jColumn: Int, newValue: ColumnVector): Matrix
    def setRowAtM(m: Matrix, iRow: Int, newValue: RowVector): Matrix = 
        m.t.setColumnAt(iRow, newValue.t)
    def setElementAtCV(cv: ColumnVector, i: Int, s: Scalar): ColumnVector
    def setElementAtRV(rv: RowVector, i: Int, s: Scalar): RowVector = 
        rv.t.setElementAt(i, s).t

    extension (m: Matrix)
        @targetName("setElementAtM_Op")
        def setElementAt(iRow: Int, jColumn: Int, newValue: Scalar): Matrix = setElementAtM(m, iRow, jColumn, newValue)
        def setColumnAt(jColumn: Int, newValue: ColumnVector): Matrix = setColumnAtM(m, jColumn, newValue)
        def setRowAt(iRow: Int, newValue: RowVector): Matrix = setRowAtM(m, iRow, newValue)

    extension (cv: ColumnVector)
        @targetName("setElementAtCV_Op")
        def setElementAt(i: Int, newValue: Scalar): ColumnVector = setElementAtCV(cv, i, newValue)
    
    extension (rv: RowVector)
        @targetName("setElementAtRV_Op")
        def setElementAt(i: Int, newValue: Scalar): RowVector = setElementAtRV(rv, i, newValue)
