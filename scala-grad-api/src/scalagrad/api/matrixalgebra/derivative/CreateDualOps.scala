package scalagrad.api.matrixalgebra.derivative

import scalagrad.api.dual

trait CreateDualScalarOps[PScalar, DScalar, DualScalar <: dual.DualScalar[PScalar, DScalar]]:

    def createDualScalar(p: PScalar, d: DScalar): DualScalar
