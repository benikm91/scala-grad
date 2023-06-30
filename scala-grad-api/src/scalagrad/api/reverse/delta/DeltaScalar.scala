package scalagrad.api.reverse.delta

enum DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] extends DeltaIndexed:
  case Zero() extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case Val(id: Int) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case ElementAtM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, jColumn: Int, nRows: Int, nCols: Int) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case ElementAtCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, length: Int) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case NegateS(s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case Invert(s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case SumCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], length: Int) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case SumM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], nRows: Int, nCols: Int) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]

  case PlusDSDS(ds1: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], ds2: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotRVDCV(rv: PRowVector, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case DotDRVCV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], cv: PColumnVector) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
  case MultiplySDS(s: PScalar, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]