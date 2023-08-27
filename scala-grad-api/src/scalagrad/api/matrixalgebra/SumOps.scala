package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait SumOps[Scalar, ColumnVector, RowVector, Matrix]:
        
    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def sumCV(cv: ColumnVector): Scalar
    def sumRV(rv: RowVector): Scalar = sumCV(rv.t)
    def sumM(m: Matrix): Scalar

    extension (cv: ColumnVector)
        @targetName("sumCV_Op")
        def sum: Scalar = sumCV(cv)

    extension (rv: RowVector)
        @targetName("sumRV_Op")
        def sum: Scalar = sumRV(rv)

    extension (m: Matrix)
        @targetName("sumM_Op")
        def sum: Scalar = sumM(m)