package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait MapOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix]
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
        with AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix] 
        with CreateOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def elementWiseOpM(m: Matrix, op: Scalar => Scalar): Matrix = 
        createMatrixFromElements(m.nRows, m.nCols, m.elements.map(op))

    def columnWiseOpM(m: Matrix, op: ColumnVector => ColumnVector): Matrix =
        createMatrixFromElements(m.nRows, m.nCols, m.columns.map(op).flatMap(_.elements))

    def rowWiseOpM(m: Matrix, op: RowVector => RowVector): Matrix = 
        createMatrixFromElements(m.nCols, m.nRows, m.rows.map(op).flatMap(_.elements)).t

    def elementWiseOpCV(cv: ColumnVector, op: Scalar => Scalar): ColumnVector = 
        createColumnVectorFromElements(cv.elements.map(op))
    
    def elementWiseOpRV(rv: RowVector, op: Scalar => Scalar): RowVector = 
        createRowVectorFromElements(rv.elements.map(op))

    extension (m: Matrix)
        @targetName("elementWiseOpM_Op_map")
        def map(op: Scalar => Scalar): Matrix = m.mapElements(op)
        @targetName("elementWiseOpM_Op")
        def mapElements(op: Scalar => Scalar): Matrix = elementWiseOpM(m, op)
        @targetName("columnWiseOpM_Op")
        def mapColumns(op: ColumnVector => ColumnVector): Matrix = columnWiseOpM(m, op)
        @targetName("rowWiseOpM_Op")
        def mapRows(op: RowVector => RowVector): Matrix = rowWiseOpM(m, op)

    extension (cv: ColumnVector)
        @targetName("elementWiseOpCV_Op_map")
        def map(op: Scalar => Scalar): ColumnVector = cv.mapElements(op)
        @targetName("elementWiseOpCV_Op")
        def mapElements(op: Scalar => Scalar): ColumnVector = elementWiseOpCV(cv, op)

    extension (rv: RowVector)
        @targetName("elementWiseOpRV_Op_map")
        def map(op: Scalar => Scalar): RowVector = rv.mapElements(op)
        @targetName("elementWiseOpRV_Op")
        def mapElements(op: Scalar => Scalar): RowVector = elementWiseOpRV(rv, op)
