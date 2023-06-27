package scalagrad.api.forward

import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{AccessOps, CreateOps, NegateOps, ScalarInvertOps, ZeroOps, SumOps, TransposeOps}
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra

case class DualNumberDerivativeMatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix](
    matrixAlgebra: MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix],
) extends DerivativeMatrixAlgebra[
    Scalar, ColumnVector, RowVector, Matrix,
    Scalar, ColumnVector, RowVector, Matrix,
    DualNumberScalar[Scalar], DualNumberColumnVector[ColumnVector], DualNumberRowVector[RowVector], DualNumberMatrix[Matrix]
]:

    override val dAccessOps: AccessOps[DScalarT, PColumnVectorT, PRowVectorT, PMatrixT] = matrixAlgebra
    override val dNegateOps: NegateOps[DScalarT, PColumnVectorT, PRowVectorT, PMatrixT] = matrixAlgebra
    override val dInvertOps: ScalarInvertOps[DScalarT] = matrixAlgebra
    override val dZeroOps: ZeroOps[DScalarT, PColumnVectorT, PRowVectorT, PMatrixT] = matrixAlgebra
    override val dTransposeOps: TransposeOps[PColumnVectorT, PRowVectorT, PMatrixT] = matrixAlgebra
    override val dCreateOps: CreateOps[DScalarT, PColumnVectorT, PRowVectorT, PMatrixT] = matrixAlgebra

    override def createDualScalar(s: PScalarT, ds: DScalarT): DualScalarT = DualNumberScalar(s, ds)

    override def createDualColumnVector(cv: PColumnVectorT, dcv: DColumnVectorT): DualColumnVectorT = DualNumberColumnVector(cv, dcv)

    override def createDualRowVector(rv: PRowVectorT, drv: DRowVectorT): DualRowVectorT = DualNumberRowVector(rv, drv)

    override def createDualMatrix(m: PMatrixT, dm: DMatrixT): DualMatrixT = DualNumberMatrix(m, dm)

    override def plusDMDM(dm1: DMatrixT, dm2: DMatrixT): DMatrixT = matrixAlgebra.plusMM(dm1, dm2)

    override def plusDMDCV(dm: DMatrixT, dcv: DColumnVectorT): DMatrixT = matrixAlgebra.plusMCV(dm, dcv)

    override def plusDMDS(dm: DMatrixT, ds: DScalarT): DMatrixT = matrixAlgebra.plusMS(dm, ds)

    override def plusDCVDCV(dcv1: DColumnVectorT, dcv2: DColumnVectorT): DColumnVectorT = matrixAlgebra.plusCVCV(dcv1, dcv2)

    override def plusDCVDS(dcv: DColumnVectorT, ds: DScalarT): DColumnVectorT = matrixAlgebra.plusCVS(dcv, ds)

    override def plusDSDS(ds1: DScalarT, ds2: DScalarT): DScalarT = matrixAlgebra.plusSS(ds1, ds2)

    override def dotMDM(m: PMatrixT, dm: PMatrixT): DMatrixT = matrixAlgebra.dotMM(m, dm)

    override def dotDMM(dm: PMatrixT, m: PMatrixT): DMatrixT = matrixAlgebra.dotMM(dm, m)

    override def dotMDCV(m: PMatrixT, dcv: DColumnVectorT): DColumnVectorT = matrixAlgebra.dotMCV(m, dcv)

    override def dotDMCV(dm: DMatrixT, cv: PColumnVectorT): DColumnVectorT = matrixAlgebra.dotMCV(dm, cv)

    override def dotDCVRV(dcv: DColumnVectorT, rv: PRowVectorT): DMatrixT = matrixAlgebra.dotCVRV(dcv, rv)

    override def dotCVDRV(cv: PColumnVectorT, drv: DRowVectorT): DMatrixT = matrixAlgebra.dotCVRV(cv, drv)

    override def dotRVDCV(rv: PRowVectorT, dcv: DColumnVectorT): DScalarT = matrixAlgebra.dotRVCV(rv, dcv)

    override def dotDRVCV(drv: DRowVectorT, cv: PColumnVectorT): DScalarT = matrixAlgebra.dotRVCV(drv, cv)

    override def multiplyMDS(m: PMatrixT, ds: DScalarT): DMatrixT = matrixAlgebra.multiplyMS(m, ds)

    override def multiplyDMS(dm: PMatrixT, s: PScalarT): DMatrixT = matrixAlgebra.multiplyMS(dm, s)

    override def multiplyCVDS(cv: PColumnVectorT, ds: DScalarT): DColumnVectorT = matrixAlgebra.multiplyCVS(cv, ds)

    override def multiplyDCVS(dcv: DColumnVectorT, s: PScalarT): DColumnVectorT = matrixAlgebra.multiplyCVS(dcv, s)

    override def multiplySDS(s: PScalarT, ds: DScalarT): DScalarT = matrixAlgebra.multiplySS(s, ds)

    override def elementWiseMultiplyMDM(m: PMatrixT, dm: DMatrixT): DMatrixT = matrixAlgebra.elementWiseMultiplyMM(m, dm)

    override def elementWiseMultiplyCVDCV(cv: PColumnVectorT, dcv: DColumnVectorT): DColumnVectorT = matrixAlgebra.elementWiseMultiplyCVCV(cv, dcv)

    override def sumCV(dcv: DColumnVectorT, length: Int): DScalarT = matrixAlgebra.sumCV(dcv)

    override def sumM(m: DMatrixT, nRows: Int, nCols: Int): DScalarT = matrixAlgebra.sumM(m)
    