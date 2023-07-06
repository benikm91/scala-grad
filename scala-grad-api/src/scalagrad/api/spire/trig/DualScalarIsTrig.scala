package scalagrad.api.spire.trig

import spire.algebra.Trig
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.CreateDualScalarOps
import scalagrad.api.dual
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.One
import spire.algebra.NRoot

object DualScalarIsTrig:

    given [
        PScalar, PColumnVector, PRowVector, PMatrix,
        DScalar, DColumnVector, DRowVector, DMatrix,
        DualScalar <: dual.DualScalar[PScalar, DScalar], 
        DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
        DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
        DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
    ](using 
        trig: Trig[PScalar],
        nRoot: NRoot[PScalar],
        matrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
        derivativeMatrixAlgebra: DerivativeMatrixAlgebra[
            PScalar, PColumnVector, PRowVector, PMatrix,
            DScalar, DColumnVector, DRowVector, DMatrix,
            DualScalar, DualColumnVector, DualRowVector, DualMatrix,
        ],
    ): Trig[DualScalar] with

        val pma = matrixAlgebra
        val dma = derivativeMatrixAlgebra

        private def chain(f: PScalar => PScalar, df: PScalar => PScalar)(dn: DualScalar) =
            dma.createDualScalar(
                f(dn.v), 
                dma.multiplySDS(df(dn.v), dn.dv),
                List(dn.dv)
            )

        private def lift(v: PScalar) = dma.createDualScalar(v, dma.dZeroOps.zeroScalar, List())

        def acos(a: DualScalar): DualScalar = 
            def dAcos(v: PScalar): PScalar = -(pma.one / nRoot.sqrt(pma.one - v * v))
            chain(trig.acos, dAcos)(a)
        def asin(a: DualScalar): DualScalar = 
            def dAsin(v: PScalar): PScalar = pma.one / nRoot.sqrt(pma.one - v * v)
            chain(trig.asin, dAsin)(a)
        def atan(a: DualScalar): DualScalar = 
            def dAtan(v: PScalar): PScalar = pma.one / (v * v + pma.one)
            chain(trig.atan, dAtan)(a)
        def atan2(y: DualScalar, x: DualScalar): DualScalar = ???
        def cos(a: DualScalar): DualScalar = 
            def dCos(v: PScalar): PScalar = -trig.sin(v)
            chain(trig.cos, dCos)(a)
        def cosh(x: DualScalar): DualScalar = 
            def dCosh(v: PScalar): PScalar = trig.sinh(v)
            chain(trig.cosh, dCosh)(x)
        def e: DualScalar = lift(trig.e)
        private def dExp(a: PScalar): PScalar = trig.exp(a)
        def exp(a: DualScalar): DualScalar = chain(trig.exp, dExp)(a)
        def expm1(a: DualScalar): DualScalar = chain(trig.expm1, dExp)(a)
        def log(a: DualScalar): DualScalar = 
            def dLog(a: PScalar): PScalar = pma.one / a
            chain(trig.log, dLog)(a)
        def log1p(a: DualScalar): DualScalar = 
            def dLog1p(a: PScalar): PScalar = pma.one / (pma.one + a)
            chain(trig.log1p, dLog1p)(a)
        def pi: DualScalar = lift(trig.pi)
        def sin(a: DualScalar): DualScalar = 
            def dSin(v: PScalar): PScalar = trig.cos(v)
            chain(trig.sin, dSin)(a)
        def sinh(x: DualScalar): DualScalar = 
            def dSinh(v: PScalar): PScalar = trig.cosh(v)
            chain(trig.sinh, dSinh)(x)
        def tan(a: DualScalar): DualScalar = 
            def dTan(v: PScalar): PScalar = pma.one / (trig.cos(v) * trig.cos(v))
            chain(trig.tan, dTan)(a)
        def tanh(x: DualScalar): DualScalar = 
            def dTanh(v: PScalar): PScalar = pma.one / (trig.cosh(v) * trig.cosh(v))
            chain(trig.tanh, dTanh)(x)
        def toDegrees(a: DualScalar): DualScalar = 
            def dToDegrees(v: PScalar): PScalar = pma.liftToScalar(180) / trig.pi
            chain(trig.toDegrees, dToDegrees)(a)
        def toRadians(a: DualScalar): DualScalar = 
            def dToRadians(v: PScalar): PScalar = trig.pi / pma.liftToScalar(180)
            chain(trig.toRadians, dToRadians)(a)