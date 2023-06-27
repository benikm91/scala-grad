package scalagrad.api.reverse.dual

import scalagrad.api.dual.DualScalar
import scalagrad.api.reverse.delta.DeltaScalar

case class DualDeltaScalar[
  PScalar, PColumnVector, PRowVector, PMatrix,
](
  value: PScalar, 
  delta: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
) 
extends DualScalar[PScalar, DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]:

  inline override def v = value
  inline override def dv = delta