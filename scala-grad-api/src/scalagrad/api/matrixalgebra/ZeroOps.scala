package scalagrad.api.matrixalgebra

import scalagrad.api.matrixalgebra.TransposeOps

trait ZeroOps[
    Scalar, ColumnVector, RowVector, Matrix,
]:

    def zeroScalar: Scalar
    def zeroColumnVector(length: Int): ColumnVector
    def zeroRowVector(length: Int): RowVector
    def zeroMatrix(nRows: Int, nCols: Int): Matrix
