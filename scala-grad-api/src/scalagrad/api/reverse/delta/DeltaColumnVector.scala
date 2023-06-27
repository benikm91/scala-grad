package scalagrad.api.reverse.delta

trait DeltaIndexed:
  private[scalagrad] var index: Int = -1

enum DeltaColumnVector[+PScalar, +PColumnVector, +PRowVector, +PMatrix] extends DeltaIndexed:
  case Zero extends DeltaColumnVector[Nothing, Nothing, Nothing, Nothing]
  case Val(id: Int) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case ColumnAtM(dAccessOpsm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], jColumn: Int) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case NegateCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case TransposeRV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case CreateCV(elements: Seq[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]

  case PlusDCVDCV(dcv1: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], dcv2: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case PlusDCVDS(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotMDCV(m: PMatrix, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotDMCV(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], cv: PColumnVector) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case MultiplyCVDS(cv: PColumnVector, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case MultiplyDCVS(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], s: PScalar) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
  case ElementWiseMultiplyCVDCV(cv: PColumnVector, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]