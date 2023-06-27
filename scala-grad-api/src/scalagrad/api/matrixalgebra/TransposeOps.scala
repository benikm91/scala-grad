package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait TransposeOps[ColumnVector, RowVector, Matrix]:

    def transpose(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("transpose_Op")
        def t: Matrix = transpose(m)

    def transposeColumVector(cv: ColumnVector): RowVector

    extension (cv: ColumnVector)
        @targetName("transposeColumnVector_Op")
        def t: RowVector = transposeColumVector(cv)

    def transposeRowVector(v: RowVector): ColumnVector
    
    extension (rv: RowVector)
        @targetName("transposeRowVector_Op")
        def t: ColumnVector = transposeRowVector(rv)