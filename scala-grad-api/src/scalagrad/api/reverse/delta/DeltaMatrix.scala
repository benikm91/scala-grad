package scalagrad.api.reverse.delta

enum DeltaMatrix[+PScalar, +PColumnVector, +PRowVector, +PMatrix] extends DeltaIndexed:
  case Zero extends DeltaMatrix[Nothing, Nothing, Nothing, Nothing]
  case Val(id: Int) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case NegateM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case TransposeM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

  case CreateM(nRows: Int, nCols: Int, elements: Seq[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case StackColumns(columns: Seq[DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

  case PlusDMDM(dm1: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], dm2: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case PlusDMDCV(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case PlusDMDS(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotMDM(m: PMatrix, dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotDMM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], m: PMatrix) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotDCVRV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], rv: PRowVector) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotCVDRV(cv: PColumnVector, drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case MultiplyMDS(m: PMatrix, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case MultiplyDMS(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], s: PScalar) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
  case ElementWiseMultiplyMDM(m: PMatrix, dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

  case SetElementAtM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, jColumn: Int, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
  case SetColumnAtM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], jColumn: Int, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], dcvLength: Int)