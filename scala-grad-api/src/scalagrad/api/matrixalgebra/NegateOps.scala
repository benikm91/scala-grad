package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait NegateOps[Scalar, ColumnVector, RowVector, Matrix]:
 
    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def negateS(s: Scalar): Scalar

    extension (s: Scalar)
        @targetName("negate_Op")
        def unary_- : Scalar = negateS(s)

    def negateCV(cv: ColumnVector): ColumnVector

    extension (cv: ColumnVector)
        @targetName("negateCV_Op")
        def unary_- : ColumnVector = negateCV(cv)

    def negateRV(rv: RowVector): RowVector = (-rv.t).t
    
    extension (rv: RowVector)
        @targetName("negateRV_Op")
        def unary_- : RowVector = negateRV(rv)
    
    def negateM(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("negateM_Op")
        def unary_- : Matrix = negateM(m)