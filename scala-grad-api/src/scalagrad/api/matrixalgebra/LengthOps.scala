package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait LengthOps[ColumnVector, RowVector, Matrix]:

    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def numberOfRows(m: Matrix): Int
    def numberOfCols(m: Matrix): Int

    extension (m: Matrix)
        @targetName("numberOfRows_Op")
        def nRows: Int = numberOfRows(m)
        @targetName("numberOfCols_Op")
        def nCols: Int = numberOfCols(m)

    def lengthColumnVector(cv: ColumnVector): Int

    extension (cv: ColumnVector)
        @targetName("lengthColumnVector_Op")
        def length: Int = lengthColumnVector(cv)

    def lengthRowVector(rv: RowVector): Int = rv.t.length

    extension (rv: RowVector)
        @targetName("lengthRowVector_Op")
        def length: Int = lengthRowVector(rv)