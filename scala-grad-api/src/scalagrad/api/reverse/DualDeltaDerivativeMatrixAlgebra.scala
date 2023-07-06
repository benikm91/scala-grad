package scalagrad.api.reverse

import scalagrad.api.reverse.delta.*
import scalagrad.api.reverse.dual.*
import scalagrad.api.matrixalgebra.{AccessOps, AccessSetOps, CreateOps, MapOps, LiftOps, NegateOps, ScalarInvertOps, ZeroOps, SumOps, TransposeOps}
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
  
trait DeltaNegateOps[PScalar, PColumnVector, PRowVector, PMatrix]()
extends NegateOps[
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
]
with DeltaTransposeOps[PScalar, PColumnVector, PRowVector, PMatrix]
:

    override def negateM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaMatrix.NegateM(m)

    override def negateCV(cv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaColumnVector.NegateCV(cv)

    override def negateRV(rv: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaRowVector.NegateRV(rv)

    override def negateS(s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.NegateS(s)

trait DeltaScalarInvertOps[PScalar, PColumnVector, PRowVector, PMatrix]()
extends ScalarInvertOps[
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
]:
    override def invert(s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.Invert(s)

trait DeltaZeroOps[PScalar, PColumnVector, PRowVector, PMatrix]()
extends ZeroOps[
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
]:
    def zeroScalar: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = DeltaScalar.Zero()
    def zeroColumnVector(length: Int): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] = DeltaColumnVector.Zero()
    def zeroRowVector(length: Int): DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix] = DeltaRowVector.Zero()
    def zeroMatrix(nRows: Int, nCols: Int): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] = DeltaMatrix.Zero()


trait DeltaTransposeOps[PScalar, PColumnVector, PRowVector, PMatrix]()
extends TransposeOps[
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
]:
    def transpose(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaMatrix.TransposeM(m)
    def transposeColumVector(cv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaRowVector.TransposeCV(cv)
    def transposeRowVector(v: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaColumnVector.TransposeRV(v)

trait DeltaCreateOps[
    PScalar, PColumnVector, PRowVector, PMatrix,
]() extends CreateOps[
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
]
with DeltaTransposeOps[PScalar, PColumnVector, PRowVector, PMatrix]
:
    def createMatrixFromElements(nRows: Int, nCols: Int, elements: Seq[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] =
        DeltaMatrix.CreateM(nRows, nCols, elements)

    def stackColumns(columns: Seq[DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]]): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaMatrix.StackColumns(columns)

    def createColumnVectorFromElements(elements: Seq[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaColumnVector.CreateCV(elements)

case class DualDeltaDerivativeMatrixAlgebra[
    PScalar, PColumnVector, PRowVector, PMatrix
]() extends DerivativeMatrixAlgebra[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
    DualDeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], 
    DualDeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], 
    DualDeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
]:

    private[scalagrad] var nextIndex: Int = 0

    private def markIndex[D <: DeltaIndexed](d: D): D =
        d.index = nextIndex
        nextIndex += 1
        d

    override val dNegateOps = new DeltaNegateOps[PScalar, PColumnVector, PRowVector, PMatrix] { }
    override val dInvertOps = new DeltaScalarInvertOps[PScalar, PColumnVector, PRowVector, PMatrix] { }
    override val dZeroOps = new DeltaZeroOps[PScalar, PColumnVector, PRowVector, PMatrix] { }
    override val dTransposeOps = new DeltaTransposeOps[PScalar, PColumnVector, PRowVector, PMatrix] { }
    override val dCreateOps = new DeltaCreateOps[PScalar, PColumnVector, PRowVector, PMatrix] { }

    override def createDualScalar(s: PScalarT, ds: DScalarT): DualScalarT = 
        DualDeltaScalar(s, markIndex(ds))

    override def createDualColumnVector(cv: PColumnVectorT, dcv: DColumnVectorT): DualColumnVectorT = 
        DualDeltaColumnVector(cv, markIndex(dcv))

    override def createDualRowVector(rv: PRowVectorT, drv: DRowVectorT): DualRowVectorT = 
        DualDeltaRowVector(rv, markIndex(drv))

    override def createDualMatrix(m: PMatrixT, dm: DMatrixT): DualMatrixT = 
        DualDeltaMatrix(m, markIndex(dm))

    override def plusDMDM(dm1: DMatrixT, dm2: DMatrixT): DMatrixT = 
        DeltaMatrix.PlusDMDM(dm1, dm2)

    override def plusDMDCV(dm: DMatrixT, dcv: DColumnVectorT): DMatrixT = 
        DeltaMatrix.PlusDMDCV(dm, dcv)

    override def plusDMDS(dm: DMatrixT, ds: DScalarT): DMatrixT =
        DeltaMatrix.PlusDMDS(dm, ds)

    override def plusDCVDCV(dcv1: DColumnVectorT, dcv2: DColumnVectorT): DColumnVectorT = 
        DeltaColumnVector.PlusDCVDCV(dcv1, dcv2)

    override def plusDCVDS(dcv: DColumnVectorT, ds: DScalarT): DColumnVectorT = 
        DeltaColumnVector.PlusDCVDS(dcv, ds)

    override def plusDSDS(ds1: DScalarT, ds2: DScalarT): DScalarT = 
        DeltaScalar.PlusDSDS(ds1, ds2)

    override def dotMDM(m: PMatrixT, dm: DMatrixT): DMatrixT = 
        DeltaMatrix.DotMDM(m, dm)

    override def dotDMM(dm: DMatrixT, m: PMatrixT): DMatrixT = 
        DeltaMatrix.DotDMM(dm, m)

    override def dotMDCV(m: PMatrixT, dcv: DColumnVectorT): DColumnVectorT = 
        DeltaColumnVector.DotMDCV(m, dcv)

    override def dotDMCV(dm: DMatrixT, cv: PColumnVectorT): DColumnVectorT = 
        DeltaColumnVector.DotDMCV(dm, cv)

    override def dotDCVRV(dcv: DColumnVectorT, rv: PRowVectorT): DMatrixT = 
        DeltaMatrix.DotDCVRV(dcv, rv)

    override def dotCVDRV(cv: PColumnVectorT, drv: DRowVectorT): DMatrixT = 
        DeltaMatrix.DotCVDRV(cv, drv)

    override def dotRVDCV(rv: PRowVectorT, dcv: DColumnVectorT): DScalarT = 
        DeltaScalar.DotRVDCV(rv, dcv)

    override def dotDRVCV(drv: DRowVectorT, cv: PColumnVectorT): DScalarT = 
        DeltaScalar.DotDRVCV(drv, cv)

    override def multiplyMDS(m: PMatrixT, ds: DScalarT): DMatrixT = 
        DeltaMatrix.MultiplyMDS(m, ds)

    override def multiplyDMS(dm: DMatrixT, s: PScalarT): DMatrixT = 
        DeltaMatrix.MultiplyDMS(dm, s)

    override def multiplyCVDS(cv: PColumnVectorT, ds: DScalarT): DColumnVectorT = 
        DeltaColumnVector.MultiplyCVDS(cv, ds)

    override def multiplyDCVS(dcv: DColumnVectorT, s: PScalarT): DColumnVectorT = 
        DeltaColumnVector.MultiplyDCVS(dcv, s)

    override def multiplySDS(s: PScalarT, ds: DScalarT): DScalarT = 
        DeltaScalar.MultiplySDS(s, ds)

    override def elementWiseMultiplyMDM(m: PMatrixT, dm: DMatrixT): DMatrixT = 
        DeltaMatrix.ElementWiseMultiplyMDM(m, dm)

    override def elementWiseMultiplyCVDCV(cv: PColumnVectorT, dcv: DColumnVectorT): DColumnVectorT = 
        DeltaColumnVector.ElementWiseMultiplyCVDCV(cv, dcv)

    override def sumCV(cv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], length: Int): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.SumCV(cv, length)

    override def sumM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], nRows: Int, nCols: Int): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.SumM(m, nRows, nCols)
    
    override def setElementAtM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, jColumn: Int, newValue: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] =
        DeltaMatrix.SetElementAtM(m, iRow, jColumn, newValue)

    override def setColumnAtM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], jColumn: Int, newValue: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], length: Int): DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix] =
        DeltaMatrix.SetColumnAtM(m, jColumn, newValue, length)

    override def setElementAtCV(cv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], i: Int, s: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] =
        DeltaColumnVector.SetElementAtCV(cv, i, s)

    override def elementAtM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, jColumn: Int, nRows: Int, nCols: Int): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.ElementAtM(m, iRow, jColumn, nRows, nCols)

    override def columnAtM(m: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], jColumn: Int, nRows: Int, nCols: Int): DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaColumnVector.ColumnAtM(m, jColumn, nRows, nCols)

    override def elementAtCV(cv: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], iRow: Int, length: Int): DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix] = 
        DeltaScalar.ElementAtCV(cv, iRow, length)
