package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait AccessOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def elementAtM(m: Matrix, iRow: Int, jColumn: Int): Scalar
    def rowAtM(m: Matrix, iRow: Int): RowVector = (m.t.columnAt(jColumn = iRow)).t
    def columnAtM(m: Matrix, jColumn: Int): ColumnVector

    def elementAtCV(cv: ColumnVector, iRow: Int): Scalar
    def elementAtRV(rv: RowVector, jColumn: Int): Scalar = rv.t.elementAt(jColumn)

    extension (m: Matrix)
        @targetName("elementAtM_Op")
        def elementAt(iRow: Int, jColumn: Int): Scalar = elementAtM(m, iRow, jColumn)
        @targetName("rowAtM_Op")
        def rowAt(iRow: Int): RowVector = rowAtM(m, iRow)
        @targetName("columnAtM_Op")
        def columnAt(jColumn: Int): ColumnVector = columnAtM(m, jColumn)

    extension (cv: ColumnVector)
        @targetName("elementAtCV_Op")
        def elementAt(iRow: Int): Scalar = elementAtCV(cv, iRow)

    extension (rv: RowVector)
        @targetName("elementAtRV_Op")
        def elementAt(jColumn: Int): Scalar = elementAtRV(rv, jColumn)