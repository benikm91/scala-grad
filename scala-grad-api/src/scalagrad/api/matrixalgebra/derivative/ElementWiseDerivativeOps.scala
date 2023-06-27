package scalagrad.api.matrixalgebra.derivative

import scalagrad.api.matrixalgebra.TransposeOps
import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait ElementWiseDerivativeOps[Scalar, ColumnVector, RowVector, Matrix]:

    def elementWiseOpM(m: Matrix, op: Scalar => Scalar, dOp: Scalar => Scalar): Matrix
    
    def columnWiseOpsM(m: Matrix, op: ColumnVector => ColumnVector, dOp: ColumnVector => Matrix): Matrix

    def rowWiseOpsM(m: Matrix, op: RowVector => RowVector, dOp: RowVector => Matrix): Matrix

    def elementWiseOpCV(cv: ColumnVector, op: Scalar => Scalar, dOp: Scalar => Scalar): ColumnVector
    
    def elementWiseOpRV(rv: RowVector, op: Scalar => Scalar, dOp: Scalar => Scalar): RowVector

    extension (m: Matrix)
        @targetName("elementWiseOpM_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): Matrix = elementWiseOpM(m, op, dOp)
        @targetName("columnWiseOpsM_Op")
        def mapColumns(op: ColumnVector => ColumnVector, dOp: ColumnVector => Matrix): Matrix = columnWiseOpsM(m, op, dOp)
        @targetName("rowWiseOpsM_Op")
        def mapRows(op: RowVector => RowVector, dOp: RowVector => Matrix): Matrix = rowWiseOpsM(m, op, dOp)

    extension (cv: ColumnVector)
        @targetName("elementWiseOpCV_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): ColumnVector = elementWiseOpCV(cv, op, dOp)

    extension (rv: RowVector)
        @targetName("elementWiseOpRV_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): RowVector = elementWiseOpRV(rv, op, dOp)