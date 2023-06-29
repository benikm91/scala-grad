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
                dma.multiplySDS(df(dn.v), dn.dv)
            )

        def acos(a: DualScalar): DualScalar = 
            def dAcos(v: PScalar): PScalar = -(pma.one / nRoot.sqrt(pma.one - v * v))
            chain(trig.acos, dAcos)(a)
        def asin(a: DualScalar): DualScalar = ???
        def atan(a: DualScalar): DualScalar = ???
        def atan2(y: DualScalar, x: DualScalar): DualScalar = ???
        def cos(a: DualScalar): DualScalar = ???
        def cosh(x: DualScalar): DualScalar = ???
        def e: DualScalar = ???
        def exp(a: DualScalar): DualScalar = ???
        def expm1(a: DualScalar): DualScalar = ???
        def log(a: DualScalar): DualScalar = 
            def dLog(a: PScalar): PScalar = pma.one / a
            chain(trig.log, dLog)(a)
        def log1p(a: DualScalar): DualScalar = ???
        def pi: DualScalar = ???
        def sin(a: DualScalar): DualScalar = ???
        def sinh(x: DualScalar): DualScalar = ???
        def tan(a: DualScalar): DualScalar = ???
        def tanh(x: DualScalar): DualScalar = ???
        def toDegrees(a: DualScalar): DualScalar = ???
        def toRadians(a: DualScalar): DualScalar = ???