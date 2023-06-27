package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix] 
    extends LengthOps[ColumnVector, RowVector, Matrix]
    with LiftOps[Scalar]
    with MatrixOps[Matrix, Scalar]
    with TransposeOps[ColumnVector, RowVector, Matrix]
    with NegateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ScalarInvertOps[Scalar]
    with ZeroOps[Scalar, ColumnVector, RowVector, Matrix]
    with BasicOps[Scalar, ColumnVector, RowVector, Matrix]
    with SumOps[Scalar, ColumnVector, RowVector, Matrix]
    with FoldLeftOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix]
    with CreateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ElementWiseOps[Scalar, ColumnVector, RowVector, Matrix]
:

    type ScalarT = Scalar
    type ColumnVectorT = ColumnVector
    type RowVectorT = RowVector
    type MatrixT = Matrix

    override def trace(m: Matrix): Scalar = 
        val n = m.nRows min m.nCols
        var res = liftToScalar(0)
        for i <- 0 until n do
            res = res + m.elementAt(i, i)
        res