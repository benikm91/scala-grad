package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait ElementWiseOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix]
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
        with AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix] 
        with CreateOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def elementWiseOpM(m: Matrix, op: Scalar => Scalar): Matrix = 
        createMatrixFromElements(m.nRows, m.nCols, m.elements.map(op))

    def columnWiseOpsM(m: Matrix, op: ColumnVector => ColumnVector): Matrix =
        createMatrixFromElements(m.nRows, m.nCols, m.columns.map(op).flatMap(_.elements))

    def rowWiseOpsM(m: Matrix, op: RowVector => RowVector): Matrix = 
        createMatrixFromElements(m.nRows, m.nCols, m.rows.map(op).flatMap(_.elements))

    def elementWiseOpCV(cv: ColumnVector, op: Scalar => Scalar): ColumnVector = 
        createColumnVectorFromElements(cv.elements.map(op))
    
    def elementWiseOpRV(rv: RowVector, op: Scalar => Scalar): RowVector = 
        createRowVectorFromElements(rv.elements.map(op))

    extension (m: Matrix)
        @targetName("elementWiseOpM_Op")
        def mapElements(op: Scalar => Scalar): Matrix = elementWiseOpM(m, op)
        @targetName("columnWiseOpsM_Op")
        def mapColumns(op: ColumnVector => ColumnVector): Matrix = columnWiseOpsM(m, op)
        @targetName("rowWiseOpsM_Op")
        def mapRows(op: RowVector => RowVector): Matrix = rowWiseOpsM(m, op)

    extension (cv: ColumnVector)
        @targetName("elementWiseOpCV_Op")
        def mapElements(op: Scalar => Scalar): ColumnVector = elementWiseOpCV(cv, op)

    extension (rv: RowVector)
        @targetName("elementWiseOpRV_Op")
        def mapElements(op: Scalar => Scalar): RowVector = elementWiseOpRV(rv, op)