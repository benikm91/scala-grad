package scalagrad.api.reverse.delta

enum DeltaRowVector[+PScalar, +PColumnVector, +PRowVector, +PMatrix] extends DeltaIndexed:
  case Zero extends DeltaRowVector[Nothing, Nothing, Nothing, Nothing]
  case Val(id: Int) extends DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case NegateRV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case TransposeCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]