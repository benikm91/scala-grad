package scalagrad.api.reverse.dual

import scalagrad.api.dual.DualMatrix
import scalagrad.api.reverse.delta.DeltaMatrix

case class DualDeltaMatrix[
  PScalar, PColumnVector, PRowVector, PMatrix,
](
  value: PMatrix, 
  delta: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
) 
extends DualMatrix[PMatrix, DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]]:

  inline override def v = value
  inline override def dv = delta