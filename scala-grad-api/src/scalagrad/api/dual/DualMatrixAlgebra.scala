package scalagrad.api.dual

import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import scalagrad.api.dual
import scalagrad.api.dual.*
import scalagrad.api.matrixalgebra.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import spire.algebra.{NRoot, Trig}
import spire.math.Numeric
import spire.syntax.nroot

import scala.reflect.TypeTest
import scala.annotation.targetName


trait DualMatrixAlgebraDSL extends MatrixAlgebraDSL:

    type PrimaryScalar
    type PrimaryColumnVector
    type PrimaryRowVector
    type PrimaryMatrix
    type DerivativeScalar
    type DerivativeColumnVector
    type DerivativeRowVector
    type DerivativeMatrix

    type Scalar <: dual.DualScalar[PrimaryScalar, DerivativeScalar] 
    type ColumnVector <: dual.DualColumnVector[PrimaryColumnVector, DerivativeColumnVector] 
    type RowVector <: dual.DualRowVector[PrimaryRowVector, DerivativeRowVector] 
    type Matrix <: dual.DualMatrix[PrimaryMatrix, DerivativeMatrix]

    val innerAlgebra: DualMatrixAlgebra[
        PrimaryScalar, PrimaryColumnVector, PrimaryRowVector, PrimaryMatrix,
        DerivativeScalar, DerivativeColumnVector, DerivativeRowVector, DerivativeMatrix,
        Scalar, ColumnVector, RowVector, Matrix,
    ]

    export innerAlgebra.primaryMatrixAlgebra
    export innerAlgebra.mapDual


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
)(using
    Numeric[PScalar],
    Trig[PScalar],
    NRoot[PScalar],
    TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualScalar],
    TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualColumnVector],
    TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualRowVector],
    TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualMatrix],
) extends MatrixAlgebra[
    DualScalar, DualColumnVector, DualRowVector, DualMatrix
]
with MapDualOps[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar, DualColumnVector, DualRowVector, DualMatrix,
](primaryMatrixAlgebra)
{
    import scalagrad.api.spire.numeric.DualScalarIsNumeric
    import scalagrad.api.spire.trig.DualScalarIsTrig

    override given num: Numeric[DualScalar] = DualScalarIsNumeric.dualNum(using
        summon[Numeric[PScalar]],
        summon[Trig[PScalar]],
        this
    )
    override given trig: Trig[DualScalar] = DualScalarIsTrig.dualTrig(using
        summon[Trig[PScalar]],
        summon[NRoot[PScalar]],
        this
    )

    private val pma = primaryMatrixAlgebra
    private val dma = derivativeMatrixAlgebra

    import dma.*

    def lift(s: Double): DualScalar = 
        createDualScalar(
            pma.lift(s),
            dma.dZeroOps.zeroScalar,
            List()
        )

    def lift(s: Float): DualScalar = 
        createDualScalar(
            pma.lift(s),
            dma.dZeroOps.zeroScalar,
            List()
        )
        
    def lift(cv: DenseVector[Double]): DualColumnVector = 
        createDualColumnVector(
            pma.lift(cv),
            dma.dZeroOps.zeroColumnVector(cv.length),
            List()
        )

    @targetName("liftCVFloat")
    def lift(cv: DenseVector[Float]): DualColumnVector = 
        createDualColumnVector(
            pma.lift(cv),
            dma.dZeroOps.zeroColumnVector(cv.length),
            List()
        )

    def lift(rv: Transpose[DenseVector[Double]]): DualRowVector = 
        createDualRowVector(
            pma.lift(rv),
            dma.dZeroOps.zeroRowVector(rv.inner.length),
            List()
        )
    
    @targetName("liftRVFloat")
    def lift(rv: Transpose[DenseVector[Float]]): DualRowVector = 
        createDualRowVector(
            pma.lift(rv),
            dma.dZeroOps.zeroRowVector(rv.inner.length),
            List()
        )

    def lift(m: DenseMatrix[Double]): DualMatrix = 
        createDualMatrix(
            pma.lift(m),
            dma.dZeroOps.zeroMatrix(m.rows, m.cols),
            List()
        )

    @targetName("liftMFloat")
    def lift(m: DenseMatrix[Float]): DualMatrix = 
        createDualMatrix(
            pma.lift(m),
            dma.dZeroOps.zeroMatrix(m.rows, m.cols),
            List()
        )

    def unlift(s: DualScalar): Double = pma.unlift(s.v)
    def unlift(cv: DualColumnVector): DenseVector[Double] = pma.unlift(cv.v)
    def unlift(rv: DualRowVector): Transpose[DenseVector[Double]] = pma.unlift(rv.v)
    def unlift(m: DualMatrix): DenseMatrix[Double] = pma.unlift(m.v)

    override def one = createDualScalar(pma.one, dma.dZeroOps.zeroScalar, List())

    override def inverse(m: MatrixT): MatrixT = 
        def dInverse(v: PMatrix, dv: DMatrix): DMatrix = 
            dotDMM(dotMDM(pma.negateM(pma.inverse(m.v)), m.dv), pma.inverse(m.v))
        createDualMatrix(
            pma.inverse(m.v),
            dInverse(m.v, m.dv),
            List(m.dv)
        )

    override def determinant(m: MatrixT): ScalarT = 
        def dDeterminant(v: PMatrix, dv: DMatrix): DScalar = 
            val inverseTransposed = pma.transpose(pma.inverse(v))
            val dDeterminant = pma.multiplyMS(inverseTransposed, pma.determinant(m.v))
            dma.sumM(elementWiseMultiplyDMM(dv, dDeterminant), m.nRows, m.nCols)
        createDualScalar(
            pma.determinant(m.v),
            dDeterminant(m.v, m.dv),
            List(m.dv)
        )
        

    override def elementAtM(m: MatrixT, iRow: Int, jColumn: Int): ScalarT = 
        createDualScalar(
            pma.elementAtM(m.v, iRow, jColumn),
            dma.elementAtM(m.dv, iRow, jColumn, pma.numberOfRows(m.v), pma.numberOfCols(m.v)), 
            List(m.dv)
        )

    override def columnAtM(m: MatrixT, jColumn: Int): ColumnVectorT = 
        createDualColumnVector(
            pma.columnAtM(m.v, jColumn),
            dma.columnAtM(m.dv, jColumn, pma.numberOfRows(m.v), pma.numberOfCols(m.v)), 
            List(m.dv)
        )

    override def elementAtCV(cv: ColumnVectorT, iRow: Int): ScalarT = 
        createDualScalar(
            pma.elementAtCV(cv.v, iRow),
            dma.elementAtCV(cv.dv, iRow, pma.lengthColumnVector(cv.v)), 
            List(cv.dv)
        )

    override def setElementAtM(m: MatrixT, iRow: Int, jColumn: Int, newValue: ScalarT): MatrixT =
        createDualMatrix(
            pma.setElementAtM(m.v, iRow, jColumn, newValue.v),
            dma.setElementAtM(m.dv, iRow, jColumn, newValue.dv), 
            List(m.dv, newValue.dv)
        )

    override def setColumnAtM(m: MatrixT, jColumn: Int, newValue: ColumnVectorT): MatrixT = 
        createDualMatrix(
            pma.setColumnAtM(m.v, jColumn, newValue.v),
            dma.setColumnAtM(m.dv, jColumn, newValue.dv, pma.lengthColumnVector(newValue.v)), 
            List(m.dv, newValue.dv)
        )

    override def setElementAtCV(cv: ColumnVectorT, i: Int, newValue: ScalarT): ColumnVectorT =
        createDualColumnVector(
            pma.setElementAtCV(cv.v, i, newValue.v),
            dma.setElementAtCV(cv.dv, i, newValue.dv), 
            List(cv.dv, newValue.dv)
        )

    override def dotMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        def dDotMM(m1: MatrixT, m2: MatrixT): DMatrix =
            plusDMDM(dotDMM(m1.dv, m2.v), dotMDM(m1.v, m2.dv))
        createDualMatrix(
            pma.dotMM(m1.v, m2.v),
            dDotMM(m1, m2),
            List(m1.dv, m2.dv)
        )

    override def dotMCV(m: MatrixT, cv: ColumnVectorT): ColumnVectorT = 
        def dDotMCV(m: MatrixT, cv: ColumnVectorT): DColumnVector = 
            plusDCVDCV(dotMDCV(m.v, cv.dv), dotDMCV(m.dv, cv.v))
        createDualColumnVector(
            pma.dotMCV(m.v, cv.v),
            dDotMCV(m, cv),
            List(m.dv, cv.dv)
        )

    override def multiplyMS(m: MatrixT, s: ScalarT): MatrixT = 
        def dMultiplyMS(m: MatrixT, s: ScalarT): DMatrix = 
            plusDMDM(multiplyMDS(m.v, s.dv), multiplyDMS(m.dv, s.v))
        createDualMatrix(
            pma.multiplyMS(m.v, s.v),
            dMultiplyMS(m, s),
            List(m.dv, s.dv)
        )

    def dDotCVRV(cv: ColumnVectorT, rv: RowVectorT): DMatrix =
        plusDMDM(dotDCVRV(cv.dv, rv.v), dotCVDRV(cv.v, rv.dv))
    override def dotCVRV(cv: ColumnVectorT, rv: RowVectorT): MatrixT = 
        createDualMatrix(
            pma.dotCVRV(cv.v, rv.v),
            dDotCVRV(cv, rv),
            List(cv.dv, rv.dv)
        )

    override def multiplyCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = 
        def dMultiplyCVS(cv: ColumnVectorT, s: ScalarT): DColumnVector = 
            plusDCVDCV(multiplyCVDS(cv.v, s.dv), multiplyDCVS(cv.dv, s.v))
        createDualColumnVector(
            pma.multiplyCVS(cv.v, s.v),
            dMultiplyCVS(cv, s),
            List(cv.dv, s.dv)
        )

    override def dotRVCV(rv: RowVectorT, cv: ColumnVectorT): ScalarT = 
        def dDotRVCV(rv: RowVectorT, cv: ColumnVectorT): DScalar = 
            plusDSDS(dotRVDCV(rv.v, cv.dv), dotDRVCV(rv.dv, cv.v))
        createDualScalar(
            pma.dotRVCV(rv.v, cv.v),
            dDotRVCV(rv, cv),
            List(rv.dv, cv.dv)
        )

    def multiplyDSS(ds: DScalar, s: PScalar): DScalar = multiplySDS(s, ds)
    override def multiplySS(s1: ScalarT, s2: ScalarT): ScalarT = 
        def dMultiplySS(s1: ScalarT, s2: ScalarT): DScalar = 
            plusDSDS(multiplyDSS(s1.dv, s2.v), multiplySDS(s1.v, s2.dv))
        createDualScalar(
            pma.multiplySS(s1.v, s2.v),
            dMultiplySS(s1, s2),
            List(s1.dv, s2.dv)
        )

    override def plusMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        createDualMatrix(
            pma.plusMM(m1.v, m2.v),
            plusDMDM(m1.dv, m2.dv),
            List(m1.dv, m2.dv)
        )

    override def plusMCV(m: MatrixT, cv: ColumnVectorT): MatrixT = 
        createDualMatrix(
            pma.plusMCV(m.v, cv.v),
            plusDMDCV(m.dv, cv.dv),
            List(m.dv, cv.dv)
        )

    override def plusMS(m: MatrixT, s: ScalarT): MatrixT = 
        createDualMatrix(
            pma.plusMS(m.v, s.v),
            plusDMDS(m.dv, s.dv),
            List(m.dv, s.dv)
        )

    override def plusCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.plusCVCV(cv1.v, cv2.v),
            plusDCVDCV(cv1.dv, cv2.dv),
            List(cv1.dv, cv2.dv)
        )

    override def plusCVS(cv: ColumnVectorT, s: ScalarT): ColumnVectorT = 
        createDualColumnVector(
            pma.plusCVS(cv.v, s.v),
            plusDCVDS(cv.dv, s.dv),
            List(cv.dv, s.dv)
        )

    override def plusSS(s1: ScalarT, s2: ScalarT): ScalarT = 
        createDualScalar(
            pma.plusSS(s1.v, s2.v),
            plusDSDS(s1.dv, s2.dv),
            List(s1.dv, s2.dv)
        )

    def elementWiseMultiplyDMM(dm: DMatrix, m: PMatrix): DMatrix = elementWiseMultiplyMDM(m, dm)
    override def elementWiseMultiplyMM(m1: MatrixT, m2: MatrixT): MatrixT = 
        def dElementWiseMultiplyMM(m1: MatrixT, m2: MatrixT): DMatrix = 
            plusDMDM(elementWiseMultiplyMDM(m1.v, m2.dv), elementWiseMultiplyDMM(m1.dv, m2.v))
        createDualMatrix(
            pma.elementWiseMultiplyMM(m1.v, m2.v),
            dElementWiseMultiplyMM(m1, m2),
            List(m1.dv, m2.dv)
        )

    def elementWiseMultiplyDCVCV(dcv: DColumnVector, cv: PColumnVector): DColumnVector = elementWiseMultiplyCVDCV(cv, dcv)
    override def elementWiseMultiplyCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): ColumnVectorT =
        def dElementWiseMultiplyCVCV(cv1: ColumnVectorT, cv2: ColumnVectorT): DColumnVector = 
            plusDCVDCV(elementWiseMultiplyDCVCV(cv1.dv, cv2.v), elementWiseMultiplyCVDCV(cv1.v, cv2.dv))
        createDualColumnVector(
            pma.elementWiseMultiplyCVCV(cv1.v, cv2.v),
            dElementWiseMultiplyCVCV(cv1, cv2),
            List(cv1.dv, cv2.dv)
        )

    override def numberOfRows(m: MatrixT): Int = pma.numberOfRows(m.v)

    override def numberOfCols(m: MatrixT): Int = pma.numberOfCols(m.v)

    override def lengthColumnVector(cv: ColumnVectorT): Int = pma.lengthColumnVector(cv.v)

    override def negateS(s: ScalarT): ScalarT = 
        createDualScalar(
            pma.negateS(s.v),
            dNegateOps.negateS(s.dv),
            List(s.dv)
        )

    override def negateCV(cv: ColumnVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.negateCV(cv.v),
            dNegateOps.negateCV(cv.dv),
            List(cv.dv)
        )

    override def negateM(m: MatrixT): MatrixT = 
        createDualMatrix(
            pma.negateM(m.v),
            dNegateOps.negateM(m.dv),
            List(m.dv)
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
            List(s.dv)
        )

    override def sumCV(cv: ColumnVectorT): ScalarT = 
        createDualScalar(
            pma.sumCV(cv.v),
            dma.sumCV(cv.dv, cv.length),
            List(cv.dv)
        )

    override def sumM(m: MatrixT): ScalarT = 
        createDualScalar(
            pma.sumM(m.v),
            dma.sumM(m.dv, m.nRows, m.nCols),
            List(m.dv)
        )

    override def createMatrixFromElements(nRows: Int, nCols: Int, elements: Seq[ScalarT]): MatrixT = 
        createDualMatrix(
            pma.createMatrixFromElements(nRows, nCols, elements.map(_.v)),
            dCreateOps.createMatrixFromElements(nRows, nCols, elements.map(_.dv)),
            elements.map(_.dv)
        )

    override def stackColumns(columns: Seq[ColumnVectorT]): MatrixT = 
        createDualMatrix(
            pma.stackColumns(columns.map(_.v)),
            dCreateOps.stackColumns(columns.map(_.dv)),
            columns.map(_.dv)
        )

    override def createColumnVectorFromElements(elements: Seq[ScalarT]): ColumnVectorT = 
        createDualColumnVector(
            pma.createColumnVectorFromElements(elements.map(_.v)),
            dCreateOps.createColumnVectorFromElements(elements.map(_.dv)),
            elements.map(_.dv)
        )

    override def transpose(m: MatrixT): MatrixT = 
        createDualMatrix(
            pma.transpose(m.v),
            dTransposeOps.transpose(m.dv),
            List(m.dv)
        )

    override def transposeColumVector(cv: ColumnVectorT): RowVectorT = 
        createDualRowVector(
            pma.transposeColumVector(cv.v),
            dTransposeOps.transposeColumVector(cv.dv),
            List(cv.dv)
        )

    override def transposeRowVector(rv: RowVectorT): ColumnVectorT = 
        createDualColumnVector(
            pma.transposeRowVector(rv.v),
            dTransposeOps.transposeRowVector(rv.dv),
            List(rv.dv)
        )


    override def zeroScalar: ScalarT = 
        createDualScalar(
            pma.zeroScalar,
            dZeroOps.zeroScalar,
            List()
        )

    override def zeroColumnVector(length: Int): ColumnVectorT = 
        createDualColumnVector(
            pma.zeroColumnVector(length),
            dZeroOps.zeroColumnVector(length),
            List()
        )

    override def zeroRowVector(length: Int): RowVectorT = 
        createDualRowVector(
            pma.zeroRowVector(length),
            dZeroOps.zeroRowVector(length),
            List()
        )

    override def zeroMatrix(nRows: Int, nCols: Int): MatrixT = 
        createDualMatrix(
            pma.zeroMatrix(nRows, nCols),
            dZeroOps.zeroMatrix(nRows, nCols),
            List()
        )

    override def elementWiseOpM(m: DualMatrix, op: PScalar => PScalar, dOp: PScalar => PScalar): DualMatrix =
        createDualMatrix(
            pma.elementWiseOpM(m.v, op),
            elementWiseMultiplyDMM(
                m.dv,
                pma.elementWiseOpM(m.v, dOp)
            ),
            List(m.dv)
        )
    
    override def columnWiseOpM(m: DualMatrix, op: PColumnVector => PColumnVector, dOp: PColumnVector => PColumnVector): DualMatrix = 
        createDualMatrix(
            pma.columnWiseOpM(m.v, op),
            elementWiseMultiplyDMM(
                m.dv,
                pma.columnWiseOpM(m.v, dOp)
            ),
            List(m.dv)
        )

    override def elementWiseOpCV(cv: DualColumnVector, op: PScalar => PScalar, dOp: PScalar => PScalar): DualColumnVector = 
        createDualColumnVector(
            pma.elementWiseOpCV(cv.v, op),
            elementWiseMultiplyDCVCV(
                cv.dv,
                pma.elementWiseOpCV(cv.v, dOp)
            ),
            List(cv.dv)
        )

    override def applyOn(ds: DualScalar, f: PScalar => PScalar, df: PScalar => PScalar): DualScalar = 
        createDualScalar(
            f(ds.v), 
            multiplySDS(df(ds.v), ds.dv),
            List(ds.dv)
        )

    def liftPrimary(xs: PMatrix): DualMatrix = 
        dma.createDualMatrix(
            xs,
            dma.dZeroOps.zeroMatrix(
                pma.numberOfRows(xs),
                pma.numberOfCols(xs)
            ),
            List(),
        )
    def liftPrimary(xs: PColumnVector): DualColumnVector = 
        dma.createDualColumnVector(
            xs,
            dma.dZeroOps.zeroColumnVector(
                pma.lengthColumnVector(xs)
            ),
            List(),
        )
    def liftPrimary(xs: PRowVector): DualRowVector = 
        dma.createDualRowVector(
            xs,
            dma.dZeroOps.zeroRowVector(
                pma.lengthRowVector(xs)
            ),
            List(),
        )
    def liftPrimary(xs: PScalar): DualScalar = 
        dma.createDualScalar(
            xs,
            dma.dZeroOps.zeroScalar,
            List(),
        )
    
}