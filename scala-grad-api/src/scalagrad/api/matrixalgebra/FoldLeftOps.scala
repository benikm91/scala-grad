package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait FoldLeftOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix]
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
        with AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix]
        with CreateOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def foldLeftM[T](m: Matrix)(s: T)(f: (T, Scalar) => T): T = 
        m.elements.foldLeft(s)(f)

    def reduceRowsM[T](m: Matrix)(f: RowVector => Scalar): ColumnVector = 
        createColumnVectorFromElements(m.rows.map(f))

    def reduceColumnsM[T](m: Matrix)(f: ColumnVector => Scalar): RowVector = 
        createRowVectorFromElements(m.columns.map(f))

    def foldLeftCV[T](cv: ColumnVector)(s: T)(f: (T, Scalar) => T): T = 
        cv.elements.foldLeft(s)(f)

    def foldLeftRV[T](rv: RowVector)(s: T)(f: (T, Scalar) => T): T =
        rv.elements.foldLeft(s)(f)

    extension (m: Matrix)
        @targetName("foldLeftM_Op")
        def foldLeft[T](s: T)(f: (T, Scalar) => T): T = foldLeftM(m)(s)(f)
        @targetName("reduceRowsM_Op")
        def reduceRows[T](f: RowVector => Scalar): ColumnVector = reduceRowsM(m)(f)
        @targetName("reduceColumnsM_Op")
        def reduceColumns[T](f: ColumnVector => Scalar): RowVector = reduceColumnsM(m)(f)

    extension (cv: ColumnVector)
        @targetName("foldLeftCV_Op")
        def foldLeft[T](s: T)(f: (T, Scalar) => T): T = foldLeftCV(cv)(s)(f)

    extension (rv: RowVector)
        @targetName("foldLeftRV_Op")
        def foldLeft[T](s: T)(f: (T, Scalar) => T): T = foldLeftRV(rv)(s)(f)