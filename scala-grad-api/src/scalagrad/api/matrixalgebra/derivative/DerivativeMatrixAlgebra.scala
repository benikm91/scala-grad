package scalagrad.api.matrixalgebra.derivative

import scalagrad.api.dual
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.AccessOps
import scalagrad.api.matrixalgebra.NegateOps
import scalagrad.api.matrixalgebra.ScalarInvertOps
import scalagrad.api.matrixalgebra.LiftOps
import scalagrad.api.matrixalgebra.TransposeOps
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.SumOps
import scalagrad.api.matrixalgebra.ZeroOps
import scalagrad.api.matrixalgebra.MapOps

trait DerivativeMatrixAlgebra[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar], 
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
]:

    type PScalarT = PScalar
    type PColumnVectorT = PColumnVector
    type PRowVectorT = PRowVector
    type PMatrixT = PMatrix
    type DScalarT = DScalar
    type DColumnVectorT = DColumnVector
    type DRowVectorT = DRowVector
    type DMatrixT = DMatrix
    type DualScalarT = DualScalar
    type DualColumnVectorT = DualColumnVector
    type DualRowVectorT = DualRowVector
    type DualMatrixT = DualMatrix

    val dNegateOps: NegateOps[DScalar, DColumnVector, DRowVector, DMatrix]
    val dInvertOps: ScalarInvertOps[DScalar]
    val dZeroOps: ZeroOps[DScalar, DColumnVector, DRowVector, DMatrix]
    val dTransposeOps: TransposeOps[DColumnVector, DRowVector, DMatrix]
    val dCreateOps: CreateOps[DScalar, DColumnVector, DRowVector, DMatrix]

    def createDualScalar(p: PScalar, d: DScalar): DualScalar
    def createDualColumnVector(p: PColumnVector, d: DColumnVector): DualColumnVector
    def createDualRowVector(p: PRowVector, d: DRowVector): DualRowVector
    def createDualMatrix(p: PMatrix, d: DMatrix): DualMatrix

    def plusDMDM(dm1: DMatrix, dm2: DMatrix): DMatrix
    def plusDMDCV(dm: DMatrix, dcv: DColumnVector): DMatrix
    def plusDMDS(dm: DMatrix, ds: DScalar): DMatrix
    def plusDCVDCV(dcv1: DColumnVector, dcv2: DColumnVector): DColumnVector
    def plusDCVDS(dcv: DColumnVector, ds: DScalar): DColumnVector
    def plusDSDS(ds1: DScalar, ds2: DScalar): DScalar

    def dotMDM(m: PMatrix, dm: DMatrix): DMatrix
    def dotDMM(dm: DMatrix, m: PMatrix): DMatrix
    def dotMDCV(m: PMatrix, dcv: DColumnVector): DColumnVector
    def dotDMCV(dm: DMatrix, cv: PColumnVector): DColumnVector
    def dotDCVRV(dcv: DColumnVector, rv: PRowVector): DMatrix
    def dotCVDRV(cv: PColumnVector, drv: DRowVector): DMatrix
    def dotRVDCV(rv: PRowVector, dcv: DColumnVector): DScalar
    def dotDRVCV(drv: DRowVector, cv: PColumnVector): DScalar
    
    def multiplyMDS(m: PMatrix, ds: DScalar): DMatrix
    def multiplyDMS(dm: DMatrix, s: PScalar): DMatrix
    def multiplyCVDS(cv: PColumnVector, ds: DScalar): DColumnVector
    def multiplyDCVS(dcv: DColumnVector, s: PScalar): DColumnVector
    def multiplySDS(s: PScalar, ds: DScalar): DScalar
    
    def elementWiseMultiplyMDM(m: PMatrix, dm: DMatrix): DMatrix
    def elementWiseMultiplyCVDCV(cv: PColumnVector, dcv: DColumnVector): DColumnVector

    def sumCV(cv: DColumnVector, length: Int): DScalar
    def sumRV(rv: DRowVector, length: Int): DScalar = sumCV(dTransposeOps.transposeRowVector(rv), length)
    def sumM(m: DMatrix, nRows: Int, nCols: Int): DScalar

    def setElementAtM(m: DMatrix, iRow: Int, jColumn: Int, s: DScalar): DMatrix
    def setColumnAtM(m: DMatrix, jColumn: Int, newValue: DColumnVector, length: Int): DMatrix
    def setElementAtCV(cv: DColumnVector, i: Int, s: DScalar): DColumnVector
    
    def elementAtM(m: DMatrix, iRow: Int, jColumn: Int, nRows: Int, nCols: Int): DScalar
    def columnAtM(m: DMatrix, jColumn: Int, nRows: Int, nCols: Int): DColumnVector
    def elementAtCV(cv: DColumnVector, iRow: Int, length: Int): DScalar 
