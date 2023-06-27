package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix]
    with TransposeOps[ColumnVector, RowVector, Matrix]
    with AccessOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def elementsM(m: Matrix): Seq[Scalar] = 
        for {
            jCol <- 0 until m.nCols
            iRow <- 0 until m.nRows
        } yield m.elementAt(iRow, jCol)

    def rowsM(m: Matrix): Seq[RowVector] =
        for (iRow <- 0 until m.nRows) yield m.rowAt(iRow)

    def columnsM(m: Matrix): Seq[ColumnVector] =
        for (jCol <- 0 until m.nCols) yield m.columnAt(jCol)

    def elementsCV(cv: ColumnVector): Seq[Scalar] =
        for (i <- 0 until cv.length) yield cv.elementAt(i)

    def elementsRV(rv: RowVector): Seq[Scalar] = rv.t.elements

    extension (m: Matrix)
        @targetName("elementsM_Op")
        def elements: Seq[Scalar] = elementsM(m)
        @targetName("rowsM_Op")
        def rows: Seq[RowVector] = rowsM(m)
        @targetName("columnsM_Op")
        def columns: Seq[ColumnVector] = columnsM(m)

    extension (cv: ColumnVector)
        @targetName("elementsCV_Op")
        def elements: Seq[Scalar] = elementsCV(cv)

    extension (rv: RowVector)
        @targetName("elementsRV_Op")
        def elements: Seq[Scalar] = elementsRV(rv)