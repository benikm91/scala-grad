package scalagrad.api.spire.trig

import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.{CreateDualScalarOps, DerivativeMatrixAlgebra}
import scalagrad.api.matrixalgebra.{MatrixAlgebra, One}
import spire.algebra.{NRoot, Trig}

object DualScalarIsTrig:

    given dualTrig[
        PScalar, PColumnVector, PRowVector, PMatrix,
        DScalar, DColumnVector, DRowVector, DMatrix,
        DualScalar <: dual.DualScalar[PScalar, DScalar], 
        DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
        DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
        DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
    ](using 
        trig: Trig[PScalar],
        nRoot: NRoot[PScalar],
        dualMa: DualMatrixAlgebra[
            PScalar, PColumnVector, PRowVector, PMatrix,
            DScalar, DColumnVector, DRowVector, DMatrix,
            DualScalar, DualColumnVector, DualRowVector, DualMatrix,
        ],
    ): Trig[DualScalar] with

        val pma = dualMa.primaryMatrixAlgebra
        val dma = dualMa.derivativeMatrixAlgebra

        import dualMa.mapDual
        import pma.*

        private def lift(v: PScalar) = dualMa.liftPrimary(v)

        def acos(a: DualScalar): DualScalar = 
            def dAcos(v: PScalar): PScalar = -(pma.one / nRoot.sqrt(pma.one - v * v))
            a.mapDual(trig.acos, dAcos)
        def asin(a: DualScalar): DualScalar = 
            def dAsin(v: PScalar): PScalar = pma.one / nRoot.sqrt(pma.one - v * v)
            a.mapDual(trig.asin, dAsin)
        def atan(a: DualScalar): DualScalar = 
            def dAtan(v: PScalar): PScalar = pma.one / (v * v + pma.one)
            a.mapDual(trig.atan, dAtan)
        def atan2(y: DualScalar, x: DualScalar): DualScalar = ???
        def cos(a: DualScalar): DualScalar = 
            def dCos(v: PScalar): PScalar = -trig.sin(v)
            a.mapDual(trig.cos, dCos)
        def cosh(a: DualScalar): DualScalar = 
            def dCosh(v: PScalar): PScalar = trig.sinh(v)
            a.mapDual(trig.cosh, dCosh)
        def e: DualScalar = lift(trig.e)
        private def dExp(a: PScalar): PScalar = trig.exp(a)
        def exp(a: DualScalar): DualScalar = a.mapDual(trig.exp, dExp)
        def expm1(a: DualScalar): DualScalar = a.mapDual(trig.expm1, dExp)
        def log(a: DualScalar): DualScalar = 
            def dLog(a: PScalar): PScalar = pma.one / a
            a.mapDual(trig.log, dLog)
        def log1p(a: DualScalar): DualScalar = 
            def dLog1p(a: PScalar): PScalar = pma.one / (pma.one + a)
            a.mapDual(trig.log1p, dLog1p)
        def pi: DualScalar = lift(trig.pi)
        def sin(a: DualScalar): DualScalar = 
            def dSin(v: PScalar): PScalar = trig.cos(v)
            a.mapDual(trig.sin, dSin)
        def sinh(a: DualScalar): DualScalar = 
            def dSinh(v: PScalar): PScalar = trig.cosh(v)
            a.mapDual(trig.sinh, dSinh)
        def tan(a: DualScalar): DualScalar = 
            def dTan(v: PScalar): PScalar = pma.one / (trig.cos(v) * trig.cos(v))
            a.mapDual(trig.tan, dTan)
        def tanh(a: DualScalar): DualScalar = 
            def dTanh(v: PScalar): PScalar = pma.one / (trig.cosh(v) * trig.cosh(v))
            a.mapDual(trig.tanh, dTanh)
        def toDegrees(a: DualScalar): DualScalar = 
            def dToDegrees(v: PScalar): PScalar = pma.lift(180) / trig.pi
            a.mapDual(trig.toDegrees, dToDegrees)
        def toRadians(a: DualScalar): DualScalar = 
            def dToRadians(v: PScalar): PScalar = trig.pi / pma.lift(180)
            a.mapDual(trig.toRadians, dToRadians)