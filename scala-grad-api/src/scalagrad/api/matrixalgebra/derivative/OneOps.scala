package scalagrad.api.matrixalgebra.derivative

import scalagrad.api.matrixalgebra.TransposeOps

trait OneOps[
    Scalar, ColumnVector, RowVector, Matrix,
]:

    def oneHotScalar: Scalar
    def oneHotColumnVector(length: Int, i: Int): ColumnVector
    def oneHotRowVector(length: Int, i: Int): RowVector
    def oneHotMatrix(nRows: Int, nCols: Int, i: Int): Matrix
