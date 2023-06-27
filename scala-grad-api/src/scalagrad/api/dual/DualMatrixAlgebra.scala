package scalagrad.api.dual

import scalagrad.api.dual.*
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.AccessOps
import scalagrad.api.matrixalgebra.NegateOps
import scalagrad.api.matrixalgebra.ScalarInvertOps
import scalagrad.api.matrixalgebra.LiftOps
import scalagrad.api.matrixalgebra.TransposeOps
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.SumOps
import scalagrad.api.dual
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra

case class DualMatrixAlgebra[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar], 
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
](
    val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
    val derivativeMatrixAlgebra: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix,
        DScalar, DColumnVector, DRowVector, DMatrix,
        DualScalar, DualColumnVector, DualRowVector, DualMatrix,
    ]
) extends MatrixAlgebra[
    DualScalar, DualColumnVector, DualRowVector, DualMatrix
] {

    import derivativeMatrixAlgebra.*

    private val pma = primaryMatrixAlgebra

    override def inverse(m: MatrixT): MatrixT = ???

    override def determinant(m: MatrixT): ScalarT = ???

    override def elementAtM(m: MatrixT, iRow: Int, jColumn: Int): ScalarT = 
        createDualScalar(
            pma.elementAtM(m.v, iRow, jColumn),
            dAccessOps.elementAtM(m.dv, iRow, jColumn),
        )

    override def columnAtM(m: MatrixT, jColumn: Int): ColumnVectorT = 
        createDualColumnVector(
            pma.columnAtM(m.v, jColumn),
            dAccessOps.columnAtM(m.dv, jColumn),
        )

    override def elementAtCV(cv: ColumnVectorT, iRow: Int): ScalarT = 
        createDualScalar(
            pma.elementAtCV(cv.v, iRow),
            dAccessOps.elementAtCV(cv.dv, iRow),
        )

    override def dotMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        def dDotMM(m1: MatrixT, m2: MatrixT): DMatrix =
            plusDMDM(dotDMM(m1.dv, m2.v), dotMDM(m1.v, m2.dv))
        createDualMatrix(
            pma.dotMM(m1.v, m2.v),
            dDotMM(m1, m2),
        )

    override def dotMCV(m: MatrixT, cv: ColumnVectorT): ColumnVectorT = 
        def dDotMCV(m: MatrixT, cv: ColumnVectorT): DColumnVector = 
            plusDCVDCV(dotMDCV(m.v, cv.dv), dotDMCV(m.dv, cv.v))
        createDualColumnVector(
            pma.dotMCV(m.v, cv.v),
            dDotMCV(m, cv),
        )

    override def multiplyMS(m: MatrixT, s: ScalarT): MatrixT = 
        def dMultiplyMS(m: MatrixT, s: ScalarT): DMatrix = 
            plusDMDM(multiplyMDS(m.v, s.dv), multiplyDMS(m.dv, s.v))
        createDualMatrix(
            pma.multiplyMS(m.v, s.v),
            dMultiplyMS(m, s),
        )

    def dDotCVRV(cv: ColumnVectorT, rv: RowVectorT): DMatrix =
        plusDMDM(dotDCVRV(cv.dv, rv.v), dotCVDRV(cv.v, rv.dv))
    override def dotCVRV(cv: ColumnVectorT, rv: RowVectorT): MatrixT = 
        createDualMatrix(
            pma.dotCVRV(cv.v, rv.v),
            dDotCVRV(cv, rv),
        )

    override def multiplyCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = 
        def dMultiplyCVS(cv: ColumnVectorT, s: ScalarT): DColumnVector = 
            plusDCVDCV(multiplyCVDS(cv.v, s.dv), multiplyDCVS(cv.dv, s.v))
        createDualColumnVector(
            pma.multiplyCVS(cv.v, s.v),
            dMultiplyCVS(cv, s),
        )

    override def dotRVCV(rv: RowVectorT, cv: ColumnVectorT): ScalarT = 
        def dDotRVCV(rv: RowVectorT, cv: ColumnVectorT): DScalar = 
            plusDSDS(dotRVDCV(rv.v, cv.dv), dotDRVCV(rv.dv, cv.v))
        createDualScalar(
            pma.dotRVCV(rv.v, cv.v),
            dDotRVCV(rv, cv),
        )

    def multiplyDSS(ds: DScalar, s: PScalar): DScalar = multiplySDS(s, ds)
    override def multiplySS(s1: ScalarT, s2: ScalarT): ScalarT = 
        def dMultiplySS(s1: ScalarT, s2: ScalarT): DScalar = 
            plusDSDS(multiplyDSS(s1.dv, s2.v), multiplySDS(s1.v, s2.dv))
        createDualScalar(
            pma.multiplySS(s1.v, s2.v),
            dMultiplySS(s1, s2),
        )

    override def plusMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        createDualMatrix(
            pma.plusMM(m1.v, m2.v),
            plusDMDM(m1.dv, m2.dv),
        )

    override def plusMCV(m: MatrixT, cv: ColumnVectorT): MatrixT = 
        createDualMatrix(
            pma.plusMCV(m.v, cv.v),
            plusDMDCV(m.dv, cv.dv),
        )

    override def plusMS(m: MatrixT, s: ScalarT): MatrixT = 
        createDualMatrix(
            pma.plusMS(m.v, s.v),
            plusDMDS(m.dv, s.dv),
        )

    override def plusCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.plusCVCV(cv1.v, cv2.v),
            plusDCVDCV(cv1.dv, cv2.dv),
        )

    override def plusCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = 
        createDualColumnVector(
            pma.plusCVS(cv.v, s.v),
            plusDCVDS(cv.dv, s.dv),
        )

    override def plusSS(s1: ScalarT, s2: ScalarT): ScalarT = 
        createDualScalar(
            pma.plusSS(s1.v, s2.v),
            plusDSDS(s1.dv, s2.dv),
        )

    def elementWiseMultiplyDMM(dm: DMatrix, m: PMatrix): DMatrix = elementWiseMultiplyMDM(m, dm)
    override def elementWiseMultiplyMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        def dElementWiseMultiplyMM(m1: MatrixT, m2: MatrixT): DMatrix = 
            plusDMDM(elementWiseMultiplyMDM(m1.v, m2.dv), elementWiseMultiplyDMM(m1.dv, m2.v))
        createDualMatrix(
            pma.elementWiseMultiplyMM(m1.v, m2.v),
            dElementWiseMultiplyMM(m1, m2),
        )

    def elementWiseMultiplyDCVCV(dcv: DColumnVector, cv: PColumnVector): DColumnVector = elementWiseMultiplyCVDCV(cv, dcv)
    override def elementWiseMultiplyCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT =
        def dElementWiseMultiplyCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): DColumnVector = 
            plusDCVDCV(elementWiseMultiplyDCVCV(cv1.dv, cv2.v), elementWiseMultiplyCVDCV(cv1.v, cv2.dv))
        createDualColumnVector(
            pma.elementWiseMultiplyCVCV(cv1.v, cv2.v),
            dElementWiseMultiplyCVCV(cv1, cv2),
        )

    override def numberOfRows(m: MatrixT): Int = pma.numberOfRows(m.v)

    override def numberOfCols(m: MatrixT): Int = pma.numberOfCols(m.v)

    override def lengthColumnVector(cv: ColumnVectorT): Int = pma.lengthColumnVector(cv.v)

    override def negateS(s: ScalarT): ScalarT = 
        createDualScalar(
            pma.negateS(s.v),
            dNegateOps.negateS(s.dv),
        )

    override def negateCV(cv: ColumnVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.negateCV(cv.v),
            dNegateOps.negateCV(cv.dv),
        )

    override def negateM(m: MatrixT): MatrixT = 
        createDualMatrix(
            pma.negateM(m.v),
            dNegateOps.negateM(m.dv),
        )

    override def invert(s: ScalarT): ScalarT = 
        def dInvert(s: ScalarT): DScalar = 
            multiplySDS(
                pma.negateS(
                    pma.invert(
                        pma.multiplySS(s.v, s.v)
                    )
                ),
                s.dv
            )
        createDualScalar(
            pma.invert(s.v),
            dInvert(s),
        )

    override def liftToScalar(d: Double): ScalarT = 
        createDualScalar(
            pma.liftToScalar(d),
            derivativeMatrixAlgebra.dZeroOps.zeroScalar,
        )

    override def sumCV(cv: ColumnVectorT): ScalarT = 
        createDualScalar(
            pma.sumCV(cv.v),
            derivativeMatrixAlgebra.sumCV(cv.dv, cv.length),
        )

    override def sumM(m: MatrixT): ScalarT = 
        createDualScalar(
            pma.sumM(m.v),
            derivativeMatrixAlgebra.sumM(m.dv, m.nRows, m.nCols),
        )

    override def createMatrixFromElements(nRows: Int, nCols: Int, elements: Seq[ScalarT]): MatrixT = 
        createDualMatrix(
            pma.createMatrixFromElements(nRows, nCols, elements.map(_.v)),
            dCreateOps.createMatrixFromElements(nRows, nCols, elements.map(_.dv)),
        )

    override def stackColumns(columns: Seq[ColumnVectorT]): MatrixT = 
        createDualMatrix(
            pma.stackColumns(columns.map(_.v)),
            dCreateOps.stackColumns(columns.map(_.dv)),
        )

    override def createColumnVectorFromElements(elements: Seq[ScalarT]): ColumnVectorT = 
        createDualColumnVector(
            pma.createColumnVectorFromElements(elements.map(_.v)),
            dCreateOps.createColumnVectorFromElements(elements.map(_.dv)),
        )

    override def transpose(m: MatrixT): MatrixT = 
        createDualMatrix(
            pma.transpose(m.v),
            dTransposeOps.transpose(m.dv),
        )

    override def transposeColumVector(cv: ColumnVectorT): RowVectorT = 
        createDualRowVector(
            pma.transposeColumVector(cv.v),
            dTransposeOps.transposeColumVector(cv.dv),
        )

    override def transposeRowVector(v: RowVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.transposeRowVector(v.v),
            dTransposeOps.transposeRowVector(v.dv),
        )


    override def zeroScalar: ScalarT = 
        createDualScalar(
            pma.zeroScalar,
            dZeroOps.zeroScalar,
        )

    override def zeroColumnVector(length: Int): ColumnVectorT = 
        createDualColumnVector(
            pma.zeroColumnVector(length),
            dZeroOps.zeroColumnVector(length),
        )

    override def zeroRowVector(length: Int): RowVectorT = 
        createDualRowVector(
            pma.zeroRowVector(length),
            dZeroOps.zeroRowVector(length),
        )

    override def zeroMatrix(nRows: Int, nCols: Int): MatrixT = 
        createDualMatrix(
            pma.zeroMatrix(nRows, nCols),
            dZeroOps.zeroMatrix(nRows, nCols),
        )

}