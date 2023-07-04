package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.DeriverPlan
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.dual

import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import scalagrad.numerical.DeriverNumericalPlan
import scalagrad.api.forward.DeriverForwardPlan
import scalagrad.api.reverse.DeriverReversePlan

import breeze.linalg.*
import scalagrad.api.matrixalgebra.MatrixAlgebra

trait IdentityTestSuit[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar],
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector],
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector],
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
](
    globalTestSuitParams: GlobalTestSuitParams[
        PScalar, PColumnVector, PRowVector, PMatrix,
        DScalar, DColumnVector, DRowVector, DMatrix,
        DualScalar, DualColumnVector, DualRowVector, DualMatrix,
    ],
    // deriver M2X
    deriverM2S: (DualMatrix => DualScalar) => (PMatrix => PMatrix),
    deriverM2CV: (DualMatrix => DualColumnVector) => (PMatrix => PMatrix),
    deriverM2RV: (DualMatrix => DualRowVector) => (PMatrix => PMatrix),
    deriverM2M: (DualMatrix => DualMatrix) => (PMatrix => PMatrix),
    // deriver CV2X
    deriverCV2S: (DualColumnVector => DualScalar) => (PColumnVector => PColumnVector),
    deriverCV2CV: (DualColumnVector => DualColumnVector) => (PColumnVector => PMatrix),
    deriverCV2RV: (DualColumnVector => DualRowVector) => (PColumnVector => PMatrix),
    deriverCV2M: (DualColumnVector => DualMatrix) => (PColumnVector => PMatrix),
    // deriver RV2X
    deriverRV2S: (DualRowVector => DualScalar) => (PRowVector => PRowVector),
    deriverRV2CV: (DualRowVector => DualColumnVector) => (PRowVector => PMatrix),
    deriverRV2RV: (DualRowVector => DualRowVector) => (PRowVector => PMatrix),
    deriverRV2M: (DualRowVector => DualMatrix) => (PRowVector => PMatrix),
    // deriver S2X
    deriverS2S: (DualScalar => DualScalar) => (PScalar => PScalar),
    deriverS2CV: (DualScalar => DualColumnVector) => (PScalar => PColumnVector),
    deriverS2RV: (DualScalar => DualRowVector) => (PScalar => PRowVector),
    deriverS2M: (DualScalar => DualMatrix) => (PScalar => PMatrix),
    // deriver AB2CD (sanity checks)
    deriverSS2SS: (Tuple2[DualScalar, DualScalar] => Tuple2[DualScalar, DualScalar]) => (Tuple2[PScalar, PScalar] => Tuple2[Tuple2[PScalar, PScalar], Tuple2[PScalar, PScalar]]),
    deriverSM2MM: (Tuple2[DualScalar, DualMatrix] => Tuple2[DualScalar, DualMatrix]) => (Tuple2[PScalar, PMatrix] => Tuple2[Tuple2[PScalar, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverCVCV2CVCV: (Tuple2[DualColumnVector, DualColumnVector] => Tuple2[DualColumnVector, DualColumnVector]) => (Tuple2[PColumnVector, PColumnVector] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverRVRV2RVRV: (Tuple2[DualRowVector, DualRowVector] => Tuple2[DualRowVector, DualRowVector]) => (Tuple2[PRowVector, PRowVector] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverCVM2CVM: (Tuple2[DualColumnVector, DualMatrix] => Tuple2[DualColumnVector, DualMatrix]) => (Tuple2[PColumnVector, PMatrix] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverRVM2RVM: (Tuple2[DualRowVector, DualMatrix] => Tuple2[DualRowVector, DualMatrix]) => (Tuple2[PRowVector, PMatrix] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverRVM2RVRV: (Tuple2[DualRowVector, DualMatrix] => Tuple2[DualRowVector, DualRowVector]) => (Tuple2[PRowVector, PMatrix] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
    deriverMM2MM: (Tuple2[DualMatrix, DualMatrix] => Tuple2[DualMatrix, DualMatrix]) => (Tuple2[PMatrix, PMatrix] => Tuple2[Tuple2[PMatrix, PMatrix], Tuple2[PMatrix, PMatrix]]),
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    override val tolerance = 1e-9

    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra

    import deriverNumericalPlan.given

    val dma = dualAlgebra
    val pma = primaryAlgebra
    import pma.*

    f"${testName} support identity" should {

        def verticalStackMatrices(ms: Seq[PMatrix]): PMatrix = 
            val (nRows, nCols) = (ms.head.nRows, ms.head.nCols)
            pma.createMatrixFromElements(nRows, nCols * 3, ms.map(_.elements).flatten)

        def horitzontalStackMatrices(ms: Seq[PMatrix]): PMatrix = 
            pma.transpose(verticalStackMatrices(ms.map(pma.transpose)))

        def eyeM(n: Int): PMatrix = eyeM2(n, n)
            
        def eyeM2(nRows: Int, nCols: Int): PMatrix = pma.createMatrixFromElements(nRows, nCols, 
            for {
                j <- 0 until nCols
                i <- 0 until nRows
            } yield if i == j then pma.one else pma.zeroScalar
        )

        def columnVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[PColumnVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                cv <- cvGen(length)
            } yield cv

        def rowVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[PRowVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                rv <- rvGen(length)
            } yield rv

        def matrixGen(minDim: Int = 1, maxDim: Int = 25): Gen[PMatrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        def mmGen(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PMatrix)] = Gen.zip(matrixGen(minDim, maxDim), matrixGen(minDim, maxDim))
        def cvcvGen(minLength: Int = 1, maxLength: Int = 25): Gen[(PColumnVector, PColumnVector)] = Gen.zip(columnVectorGen(minLength, maxLength), columnVectorGen(minLength, maxLength))
        def rvrvGen(minLength: Int = 1, maxLength: Int = 25): Gen[(PRowVector, PRowVector)] = Gen.zip(rowVectorGen(minLength, maxLength), rowVectorGen(minLength, maxLength))
        def ssGen: Gen[(PScalar, PScalar)] = Gen.zip(sGen, sGen)
        
        "one to one" should {
            "support for S2S" in {
                def f(s: dma.ScalarT): dma.ScalarT = s
                def realDf(s: pma.ScalarT): pma.ScalarT = pma.one
                forAll(sGen) { (s) =>
                    deriverS2S(f)(s) should be (realDf(s))
                }
            }
            "support for S2CV" in {
                def f(s: dma.ScalarT): dma.ColumnVectorT = dma.createColumnVectorFromElements(Seq(s, s, dma.zeroScalar))
                def realDf(s: pma.ScalarT) = pma.createColumnVectorFromElements(Seq(pma.one, pma.one, pma.zeroScalar))
                forAll(sGen) { (s) =>
                    deriverS2CV(f)(s) should be (realDf(s))
                }
            }
            "support for S2RV" in {
                def f(s: dma.ScalarT): dma.RowVectorT = dma.createRowVectorFromElements(Seq(s, s, dma.zeroScalar))
                def realDf(s: pma.ScalarT) = pma.createRowVectorFromElements(Seq(pma.one, pma.one, pma.zeroScalar))
                forAll(sGen) { (s) =>
                    deriverS2RV(f)(s) should be (realDf(s))
                }
            }
            "support for S2M" in {
                def f(s: dma.ScalarT): dma.MatrixT = dma.createMatrixFromElements(2, 3, Seq(s, s, dma.zeroScalar, s, dma.zeroScalar, s))
                def realDf(s: pma.ScalarT) = pma.createMatrixFromElements(2, 3, Seq(pma.one, pma.one, pma.zeroScalar, pma.one, pma.zeroScalar, pma.one))
                forAll(sGen) { (s) =>
                    deriverS2M(f)(s) should be (realDf(s))
                }
            }
            "support for CV2S" in {
                def f(cv: dma.ColumnVectorT): dma.ScalarT = { import dma.*; cv.elementAt(0) }
                def realDf(cv: pma.ColumnVectorT): pma.ColumnVectorT = pma.zeroColumnVector(cv.length).setElementAt(0, pma.one)
                forAll(columnVectorGen()) { (cv) =>
                    deriverCV2S(f)(cv) should be (realDf(cv))
                }
            }
            "support for CV2CV" in {
                def f(cv: dma.ColumnVectorT): dma.ColumnVectorT = cv
                def realDf(cv: pma.ColumnVectorT): pma.MatrixT = eyeM(cv.length)
                forAll(columnVectorGen()) { (cv) =>
                    deriverCV2CV(f)(cv) should be (realDf(cv))
                }
            }
            "support for CV2CV (non-diagonal)" in {
                def f(cv: dma.ColumnVectorT): dma.ColumnVectorT = 
                    import dma.*;
                    cv.setElementAt(0, cv.elementAt(0) + cv.elementAt(1)) // add non diagonal relation
                def realDf(cv: pma.ColumnVectorT): pma.MatrixT = 
                    import pma.*;
                    val res = eyeM(cv.length)
                    res.setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                forAll(columnVectorGen(minLength=2)) { (cv) =>
                    deriverCV2CV(f)(cv) should be (realDf(cv))
                }
            }
            "support for CV2RV" in {
                def f(cv: dma.ColumnVectorT): dma.RowVectorT = { import dma.*; cv.t }
                def realDf(cv: pma.ColumnVectorT): pma.MatrixT = eyeM(cv.length)
                forAll(columnVectorGen()) { (cv) =>
                    deriverCV2RV(f)(cv) should be (realDf(cv))
                }
            }
            "support for CV2RV (non-diagonal)" in {
                def f(cv: dma.ColumnVectorT): dma.RowVectorT = { 
                    import dma.*; 
                    val res = cv.t 
                    res.setElementAt(0, res.elementAt(0) + cv.elementAt(1)) // add non diagonal relation
                }
                def realDf(cv: pma.ColumnVectorT): pma.MatrixT = 
                    val res = eyeM(cv.length)
                    res.setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                forAll(columnVectorGen(minLength = 2, maxLength = 2)) { (cv) =>
                    deriverCV2RV(f)(cv) should be (realDf(cv))
                }
            }
            "support for CV2M" in {
                def f(cv: dma.ColumnVectorT): dma.MatrixT = dma.stackColumns(Seq(cv, cv, cv))
                def realDf(cv: pma.ColumnVectorT): pma.MatrixT = verticalStackMatrices(List(eyeM(cv.length), eyeM(cv.length), eyeM(cv.length)))
                forAll(columnVectorGen()) { (cv) =>
                    deriverCV2M(f)(cv) should be (realDf(cv))
                }
            }
            "support for RV2S" in {
                def f(rv: dma.RowVectorT): dma.ScalarT = { import dma.*; rv.elementAt(0) }
                def realDf(rv: pma.RowVectorT): pma.RowVectorT = pma.zeroRowVector(rv.length).setElementAt(0, pma.one)
                forAll(rowVectorGen()) { (rv) =>
                    deriverRV2S(f)(rv) should be (realDf(rv))
                }
            }
            "support for RV2CV" in {
                def f(rv: dma.RowVectorT): dma.ColumnVectorT = { import dma.*; rv.t }
                def realDf(rv: pma.RowVectorT): pma.MatrixT = eyeM(rv.length)
                forAll(rowVectorGen()) { (rv) =>
                    deriverRV2CV(f)(rv) should be (realDf(rv))
                }
            }
            "support for RV2CV (non-diagonal)" in {
                def f(rv: dma.RowVectorT): dma.ColumnVectorT = { 
                    import dma.*; 
                    val res = rv.t
                    res.setElementAt(0, res.elementAt(0) + rv.elementAt(1)) // add non diagonal relation 
                }
                def realDf(rv: pma.RowVectorT): pma.MatrixT = 
                    import pma.*;
                    val res = eyeM(rv.length)
                    res.setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                forAll(rowVectorGen(minLength=2)) { (rv) =>
                    deriverRV2CV(f)(rv) should be (realDf(rv))
                }
            }
            "support for RV2RV" in {
                def f(rv: dma.RowVectorT): dma.RowVectorT = rv
                def realDf(rv: pma.RowVectorT): pma.MatrixT = eyeM(rv.length)
                forAll(rowVectorGen()) { (rv) =>
                    deriverRV2RV(f)(rv) should be (realDf(rv))
                }
            }
            "support for RV2RV (non-diagonal)" in {
                def f(rv: dma.RowVectorT): dma.RowVectorT = 
                    import dma.*;
                    val res = rv
                    res.setElementAt(0, res.elementAt(0) + rv.elementAt(1)) // add non diagonal relation
                def realDf(rv: pma.RowVectorT): pma.MatrixT = 
                    import pma.*;
                    val res = eyeM(rv.length)
                    res.setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                forAll(rowVectorGen(minLength=2)) { (rv) =>
                    deriverRV2RV(f)(rv) should be (realDf(rv))
                }
            }
            "support for RV2M" in {
                def f(rv: dma.RowVectorT): dma.MatrixT = 
                    dma.stackRows(Seq(rv, rv, rv))
                def realDf(rv: pma.RowVectorT): pma.MatrixT = 
                    pma.createMatrixFromElements(rv.length, rv.length * 3, 
                        for {
                            colJ <- 0 until rv.length * 3
                            rowI <- 0 until rv.length
                        } yield if rowI == Math.floor(colJ / 3) then pma.one else pma.zeroScalar
                    )
                forAll(rowVectorGen()) { (rv) =>
                    deriverRV2M(f)(rv) should be (realDf(rv))
                }
            }
            "support for M2S" in {
                def f(m: dma.MatrixT): dma.ScalarT = { import dma.*; m.elementAt(0, 0) }
                def realDf(m: pma.MatrixT): pma.MatrixT = pma.zeroMatrix(m.nRows, m.nCols).setElementAt(0, 0, pma.one)
                forAll(matrixGen()) { (m) =>
                    deriverM2S(f)(m) should be (realDf(m))
                }
            }
            "support for M2CV" in {
                def f(m: dma.MatrixT): dma.ColumnVectorT = { import dma.*; m.columnAt(0) }
                def realDf(m: pma.MatrixT): pma.MatrixT = eyeM2(m.nRows * m.nCols, m.nRows)
                forAll(matrixGen()) { (m) =>
                    deriverM2CV(f)(m) should be (realDf(m))
                }
            }
            "support for M2RV" in {
                def f(m: dma.MatrixT): dma.RowVectorT = { import dma.*; m.rowAt(0) }
                def realDf(m: pma.MatrixT): pma.MatrixT = eyeM2(m.nRows * m.nCols, m.nCols)
                forAll(matrixGen(maxDim=2)) { (m) =>
                    deriverM2RV(f)(m) should be (realDf(m))
                }
            }
            "support for M2M" in {
                def f(m: dma.MatrixT): dma.MatrixT = m
                def realDf(m: pma.MatrixT): pma.MatrixT = eyeM(m.nRows * m.nCols)
                forAll(matrixGen()) { (m) =>
                    deriverM2M(f)(m) should be (realDf(m))
                }
            }
            "support for M2M (non-diagonal)" in {
                def f(m: dma.MatrixT): dma.MatrixT =
                    import dma.*
                    m.setElementAt(0, 0, m.elementAt(0, 0) + m.elementAt(1, 0))
                def realDf(m: pma.MatrixT): pma.MatrixT = 
                    val res = eyeM(m.nRows * m.nCols)
                    res.setElementAt(1, 0, pma.one)
                forAll(matrixGen(minDim=2, maxDim=2)) { (m) =>
                    deriverM2M(f)(m) should be (realDf(m))
                }
            }
        }
        "two to two" should {
            "support for SS2SS" in {
                def f(s1: dma.ScalarT, s2: dma.ScalarT): (dma.ScalarT, dma.ScalarT) = (s1, s2)
                def realDf(s1: pma.ScalarT, s2: pma.ScalarT) = ((pma.one, pma.zeroScalar), (pma.zeroScalar, pma.one))
                forAll(ssGen) { (s1, s2) =>
                    deriverSS2SS(f.tupled)(s1, s2) should be(realDf(s1, s2))
                }
            }
            "support for SM2MM" in {
                def f(s: dma.ScalarT, m: dma.MatrixT): (dma.ScalarT, dma.MatrixT) = (s, m)
                def realDf(s: pma.ScalarT, m: pma.MatrixT) = 
                    ((pma.one, pma.zeroMatrix(m.nRows, m.nCols)), (pma.zeroMatrix(m.nRows, m.nCols), eyeM2(m.nRows * m.nCols, m.nRows * m.nCols)))
                forAll(Gen.zip(sGen, matrixGen(maxDim=2))) { (s, m) =>
                    deriverSM2MM(f.tupled)(s, m) should be(realDf(s, m))
                }
            }
            "support for CVCV2CVCV" in {
                def f(cv1: dma.ColumnVectorT, cv2: dma.ColumnVectorT): (dma.ColumnVectorT, dma.ColumnVectorT) = (cv1, cv2)
                def realDf(cv1: pma.ColumnVectorT, cv2: pma.ColumnVectorT) = 
                    ((eyeM(cv1.length), pma.zeroMatrix(cv1.length, cv2.length)), (pma.zeroMatrix(cv2.length, cv1.length), eyeM(cv2.length)))
                forAll(cvcvGen()) { (cv1, cv2) =>
                    deriverCVCV2CVCV(f.tupled)(cv1, cv2) should be(realDf(cv1, cv2))
                }
            }
            "support for CVM2CVM" in {
                def f(cv: dma.ColumnVectorT, m: dma.MatrixT): (dma.ColumnVectorT, dma.MatrixT) = (cv, m)
                def realDf(cv: pma.ColumnVectorT, m: pma.MatrixT) = 
                    val numberOfInputsCV = cv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsCV = cv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM2(numberOfInputsCV, numberOfOutputsCV), pma.zeroMatrix(numberOfInputsCV, numberOfOutputsM)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsCV), eyeM2(numberOfInputsM, numberOfOutputsM)))

                forAll(Gen.zip(columnVectorGen(), matrixGen())) { (cv, m) =>
                    deriverCVM2CVM(f.tupled)(cv, m) should be(realDf(cv, m))
                }
            }
            "support for RVRV2RVRV" in {
                def f(rv1: dma.RowVectorT, rv2: dma.RowVectorT): (dma.RowVectorT, dma.RowVectorT) = (rv1, rv2)
                def realDf(rv1: pma.RowVectorT, rv2: pma.RowVectorT) = ((eyeM(rv1.length), pma.zeroMatrix(rv1.length, rv2.length)), (pma.zeroMatrix(rv2.length, rv1.length), eyeM(rv2.length)))
                forAll(rvrvGen()) { (rv1, rv2) =>
                    deriverRVRV2RVRV(f.tupled)(rv1, rv2) should be(realDf(rv1, rv2))
                }
            }
            "support for RVM2RVM" in {
                def f(rv: dma.RowVectorT, m: dma.MatrixT): (dma.RowVectorT, dma.MatrixT) = (rv, m)
                def realDf(rv: pma.RowVectorT, m: pma.MatrixT) = 
                    val numberOfInputsRV = rv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsRV = rv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM2(numberOfInputsRV, numberOfOutputsRV), pma.zeroMatrix(numberOfInputsRV, numberOfOutputsM)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV), eyeM2(numberOfInputsM, numberOfOutputsM)))

                forAll(Gen.zip(rowVectorGen(), matrixGen())) { (rv, m) =>
                    deriverRVM2RVM(f.tupled)(rv, m) should be(realDf(rv, m))
                }
            }
            "support for RVM2RVRV" in {
                def f(rv: dma.RowVectorT, m: dma.MatrixT): (dma.RowVectorT, dma.RowVectorT) = (rv, rv)
                def realDf(rv: pma.RowVectorT, m: pma.MatrixT) = 
                    val numberOfInputsRV = rv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsRV = rv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM(rv.length), eyeM(rv.length)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV), pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV)))
                forAll(Gen.zip(rowVectorGen(), matrixGen())) { (rv, m) =>
                    deriverRVM2RVRV(f.tupled)(rv, m) should be(realDf(rv, m))
                }
            }
            "support for MM2MM" in {
                def f(m1: dma.MatrixT, m2: dma.MatrixT): (dma.MatrixT, dma.MatrixT) = (m1, m2)
                def realDf(m1: pma.MatrixT, m2: pma.MatrixT) = 
                    val numberOfInputsM1 = m1.nRows * m1.nCols
                    val numberOfInputsM2 = m2.nRows * m2.nCols
                    val numberOfOutputsM1 = m1.nRows * m1.nCols
                    val numberOfOutputsM2 = m2.nRows * m2.nCols
                    ((eyeM2(numberOfInputsM1, numberOfOutputsM1), pma.zeroMatrix(numberOfInputsM1, numberOfOutputsM2)), (pma.zeroMatrix(numberOfInputsM2, numberOfOutputsM1), eyeM2(numberOfInputsM2, numberOfOutputsM2)))

                forAll(mmGen(maxDim=2)) { (m1, m2) =>
                    val ((a1, a2), (a3, a4)) = deriverMM2MM(f.tupled)(m1, m2) 
                    val ((b1, b2), (b3, b4)) = realDf(m1, m2)
                    println("***")
                    println(m1.nRows + " " + m1.nCols)
                    println(m2.nRows + " " + m2.nCols)
                    println("**")
                    println(a1.nRows + " " + a1.nCols)
                    println(b1.nRows + " " + b1.nCols)
                    println("**")
                    println(a2.nRows + " " + a2.nCols)
                    println(b2.nRows + " " + b2.nCols)
                    println("**")
                    println(a3.nRows + " " + a3.nCols)
                    println(b3.nRows + " " + b3.nCols)
                    println("**")
                    println(a4.nRows + " " + a4.nCols)
                    println(b4.nRows + " " + b4.nCols)
                    deriverMM2MM(f.tupled)(m1, m2) should be(realDf(m1, m2))
                }
            }
        }
    }

