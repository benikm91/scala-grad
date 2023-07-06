package scalagrad.api.reverse.delta

enum DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]:
  case Zero()
  case Input(id: Int)
  case Val(index: Int, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] with DeltaVal
  case ColumnAtM(dAccessOpsm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], jColumn: Int, nRows: Int, nCols: Int)
  case NegateCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case TransposeRV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case CreateCV(elements: Seq[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]])

  case PlusDCVDCV(dcv1: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], dcv2: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case PlusDCVDS(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
  case DotMDCV(m: PMatrix, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case DotDMCV(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], cv: PColumnVector)
  case MultiplyCVDS(cv: PColumnVector, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
  case MultiplyDCVS(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], s: PScalar)
  case ElementWiseMultiplyCVDCV(cv: PColumnVector, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])

  case SetElementAtCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], i: Int, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])