package scalagrad.api.spire.numeric

import cats.kernel.Order
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import spire.algebra.Trig
import spire.math.Numeric

object DualScalarIsNumeric:

    given dualNum[
        PScalar, PColumnVector, PRowVector, PMatrix,
        DScalar, DColumnVector, DRowVector, DMatrix,
        DualScalar <: dual.DualScalar[PScalar, DScalar], 
        DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
        DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
        DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
    ](using 
        num: Numeric[PScalar],
        trig: Trig[PScalar],
        dualMatrixAlgebra: DualMatrixAlgebra[
            PScalar, PColumnVector, PRowVector, PMatrix,
            DScalar, DColumnVector, DRowVector, DMatrix,
            DualScalar, DualColumnVector, DualRowVector, DualMatrix,
        ],
    ): Numeric[DualScalar] with {

        private val dualMa = dualMatrixAlgebra
        private val pma = dualMatrixAlgebra.primaryMatrixAlgebra
        private val dma = dualMatrixAlgebra.derivativeMatrixAlgebra

        import dualMa.mapDual
        import pma.*

        private def lift(v: PScalar) = dualMa.liftPrimary(v)

        def negate(x: DualScalar): DualScalar = dualMa.negateS(x)
        def one: DualScalar = dualMa.one
        def zero: DualScalar = dualMa.zeroScalar
        def plus(x: DualScalar, y: DualScalar): DualScalar = dualMa.plusSS(x, y)
        def toAlgebraic(a: DualScalar): spire.math.Algebraic = num.toAlgebraic(a.v)
        def toBigDecimal(a: DualScalar): BigDecimal = num.toBigDecimal(a.v)
        def toBigInt(a: DualScalar): BigInt = num.toBigInt(a.v)
        def toByte(a: DualScalar): Byte = num.toByte(a.v)
        def toDouble(a: DualScalar): Double = num.toDouble(a.v)
        def toFloat(a: DualScalar): Float = num.toFloat(a.v)
        def toInt(a: DualScalar): Int = num.toInt(a.v)
        def toLong(a: DualScalar): Long= num.toLong(a.v)
        def toNumber(a: DualScalar): spire.math.Number = num.toNumber(a.v)
        def toRational(a: DualScalar): spire.math.Rational = num.toRational(a.v)
        def toReal(a: DualScalar): spire.math.Real = num.toReal(a.v)
        def toShort(a: DualScalar): Short = num.toShort(a.v)
        def toString(a: DualScalar): String = num.toString(a.v)
        def toType[B](a: DualScalar)(implicit evidence$1: spire.math.ConvertableTo[B]): B = num.toType(a.v)
        def fromAlgebraic(n: spire.math.Algebraic): DualScalar = lift(num.fromAlgebraic(n))
        def fromBigDecimal(n: BigDecimal): DualScalar = lift(num.fromBigDecimal(n))
        def fromByte(n: Byte): DualScalar = lift(num.fromByte(n))
        def fromDouble(n: Double): DualScalar = lift(num.fromDouble(n))
        def fromFloat(n: Float): DualScalar = lift(num.fromFloat(n))
        def fromLong(n: Long): DualScalar = lift(num.fromLong(n))
        def fromRational(n: spire.math.Rational): DualScalar = lift(num.fromRational(n))
        def fromReal(n: spire.math.Real): DualScalar = lift(num.fromReal(n))
        def fromShort(n: Short): DualScalar = lift(num.fromShort(n))
        def fromType[B](b: B)(implicit evidence$2: spire.math.ConvertableFrom[B]): DualScalar = lift(num.fromType(b))
        def ceil(a: DualScalar): DualScalar = 
            def dCeil(v: PScalar): PScalar = 
                return pma.zeroScalar
            a.mapDual(num.ceil, dCeil)
        def floor(a: DualScalar): DualScalar = 
            def dFloor(v: PScalar): PScalar = 
                return pma.zeroScalar
            a.mapDual(num.ceil, dFloor)
        def isWhole(a: DualScalar): Boolean = num.isWhole(a.v)
        def round(a: DualScalar): DualScalar = 
            def dRound(v: PScalar): PScalar = 
                return num.zero
            a.mapDual(num.round, dRound)
        def div(x: DualScalar, y: DualScalar): DualScalar = dualMa.divideSS(x, y)
        def times(x: DualScalar, y: DualScalar): DualScalar = dualMa.multiplySS(x, y)
        def fpow(a: DualScalar, b: DualScalar): DualScalar = 
            def log(x: PScalar): PScalar = trig.log(x)
            val dfpow = dma.multiplySDS(
                num.fpow(a.v, b.v - pma.one),
                dma.plusDSDS(
                    dma.multiplySDS(b.v, a.dv), 
                    dma.multiplySDS(a.v * trig.log(a.v), b.dv)
                )
            )
            dma.createDualScalar(num.fpow(a.v, b.v), dfpow, List(a.dv, b.dv))
        def nroot(a: DualScalar, n: Int): DualScalar = 
            def dnroot(x: PScalar) = 
                val denominator = pma.multiplySS(pma.lift(n), num.fpow((num.nroot(x, n)), num.fromInt(n - 1)))
                num.one / denominator
            a.mapDual(x => num.nroot(x, n), dnroot)
        def compare(x: DualScalar, y: DualScalar): Int = num.compare(x.v, y.v)
        def abs(a: DualScalar): DualScalar = 
            def dAbs(v: PScalar): PScalar = 
                num.fromInt(num.signum(v))
            a.mapDual(num.abs, dAbs)
        def additiveCommutativeMonoid: algebra.ring.AdditiveCommutativeMonoid[DualScalar] = ???
        def order: cats.kernel.Order[DualScalar] = new Order {
          override def compare(x: DualScalar, y: DualScalar): Int = num.order.compare(x.v, y.v)
        }
        def signum(a: DualScalar): Int = num.signum(a.v)

    }