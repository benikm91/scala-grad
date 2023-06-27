package scalagrad.api.reverse.dual

import scalagrad.api.dual.DualColumnVector
import scalagrad.api.reverse.delta.DeltaColumnVector

case class DualDeltaColumnVector[
  PScalar, PColumnVector, PRowVector, PMatrix,
](
  value: PColumnVector, 
  delta: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
) 
extends DualColumnVector[PColumnVector, DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]]:

  inline override def v = value
  inline override def dv = delta