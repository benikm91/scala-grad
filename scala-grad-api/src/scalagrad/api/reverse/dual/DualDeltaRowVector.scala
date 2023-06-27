package scalagrad.api.reverse.dual

import scalagrad.api.dual.DualRowVector
import scalagrad.api.reverse.delta.DeltaRowVector

case class DualDeltaRowVector[
  PScalar, PColumnVector, PRowVector, PMatrix,
](
  value: PRowVector, 
  delta: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
) 
extends DualRowVector[PRowVector, DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]]:

  inline override def v = value
  inline override def dv = delta