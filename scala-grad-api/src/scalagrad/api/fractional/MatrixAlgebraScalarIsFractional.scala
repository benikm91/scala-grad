package scalagrad.api.fractional

import scalagrad.api.matrixalgebra.MatrixAlgebra
import spire.math.Numeric
import scalagrad.api.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra

object MatrixAlgebraScalarIsFractional:

    given [Scalar, ColumnVector, RowVector, Matrix](using 
        ma: MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix]
    ): Fractional[Scalar] with
        def compare(x1: Scalar, x2: Scalar): Int = x1.toDouble compare x2.toDouble
        def div(x: Scalar, y: Scalar): Scalar = ma.divideSS(x, y)
        def fromInt(x: Int): Scalar = ma.lift(x)
        def minus(x: Scalar, y: Scalar): Scalar = ma.minusSS(x, y)
        def negate(x: Scalar): Scalar = ma.negateS(x)
        def parseString(str: String): Option[Scalar] = ???
        def plus(x: Scalar, y: Scalar): Scalar = ma.plusSS(x, y)
        def times(x: Scalar, y: Scalar): Scalar = ma.multiplySS(x, y)
        def toDouble(x: Scalar): Double = ma.unlift(x)
        def toFloat(x: Scalar): Float = toDouble(x).toFloat
        def toInt(x: Scalar): Int = toDouble(x).toInt
        def toLong(x: Scalar): Long = toDouble(x).toLong