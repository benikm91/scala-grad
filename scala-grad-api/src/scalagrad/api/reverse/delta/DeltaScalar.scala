package scalagrad.api.reverse.delta

trait DeltaVal:
  def index: Int

enum DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]:
  case Zero()
  case Input(id: Int)
  case Val(index: Int, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]) extends DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] with DeltaVal
  case ElementAtM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, jColumn: Int, nRows: Int, nCols: Int)
  case ElementAtCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, length: Int)
  case NegateS(s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
  case SumCV(dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], length: Int)
  case SumM(dm: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], nRows: Int, nCols: Int)

  case PlusDSDS(ds1: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], ds2: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
  case DotRVDCV(rv: PRowVector, dcv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix])
  case DotDRVCV(drv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], cv: PColumnVector)
  case MultiplySDS(s: PScalar, ds: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix])
