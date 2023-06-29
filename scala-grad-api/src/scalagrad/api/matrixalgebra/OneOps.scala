package scalagrad.api.matrixalgebra

trait OneOps[
    Scalar, ColumnVector, RowVector, Matrix,
]:
    
    this: 
        One[Scalar]
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with ZeroOps[Scalar, ColumnVector, RowVector, Matrix]
        with AccessSetOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def oneHotColumnVector(length: Int, i: Int): ColumnVector = 
        val cv = zeroColumnVector(length)
        cv.setElementAt(i, one)

    def oneHotRowVector(length: Int, i: Int): RowVector =
        val rv = zeroRowVector(length)
        rv.setElementAt(i, one)

    def oneHotMatrix(nRows: Int, nCols: Int, i: Int): Matrix =
        val m = zeroMatrix(nRows, nCols)
        m.setElementAt(i % nRows, i / nRows, one)
