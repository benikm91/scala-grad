package scalagrad.api.reverse.delta

enum DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]:
  case Zero()
  case Input(id: Int)
  case Val(index: Int, drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix] with DeltaVal
  case NegateRV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case TransposeCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])