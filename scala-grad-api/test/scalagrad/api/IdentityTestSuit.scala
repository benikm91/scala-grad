package scalagrad.api.test

import scalagrad.api.ScalaGrad
import scalagrad.api.Mode
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

import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.ModeO

/**
 * Functions can have a lot of different structures, like Scalar => Matrix or (Scalar, Scalar) => (Scalar, Scalar).
 * 
 * A deriver must support all combinations of input and output types.
 * 
 * This test suit tests:
 *  - the basic combinations of one input type to one output type
 *  - some complex combinations of two input types to two output types
 * 
 * For testing mostly the identity operation is used. 
 * However there are some cases adding non-diagonal relations to the solution.
 * Non-diagonality helps for example to detect errors of wrongly transposed results.
 */
trait IdentityTestSuit(
    val deriver: ModeO,
    val pma: MatrixAlgebraDSL,
    globalTestSuitParams: GlobalTestSuitParams[pma.Scalar, pma.ColumnVector, pma.RowVector, pma.Matrix],
) extends AnyWordSpec with BaseTestSuit[pma.Scalar, pma.ColumnVector, pma.RowVector, pma.Matrix]:

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    import globalTestSuitParams.*

    f"${testName} support identity" should {

        def verticalStackMatrices(ms: Seq[pma.Matrix]): pma.Matrix = 
            val (nRows, nCols) = (ms.head.nRows, ms.head.nCols)
            pma.createMatrixFromElements(nRows, nCols * 3, ms.map(_.elements).flatten)

        def horitzontalStackMatrices(ms: Seq[pma.Matrix]): pma.Matrix = 
            pma.transpose(verticalStackMatrices(ms.map(pma.transpose)))

        def eyeM(n: Int): pma.Matrix = eyeM2(n, n)
            
        def eyeM2(nRows: Int, nCols: Int): pma.Matrix = pma.createMatrixFromElements(nRows, nCols, 
            for {
                j <- 0 until nCols
                i <- 0 until nRows
            } yield if i == j then pma.one else pma.zeroScalar
        )

        def columnVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[pma.ColumnVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                cv <- cvGen(length)
            } yield cv

        def rowVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[pma.RowVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                rv <- rvGen(length)
            } yield rv

        def matrixGen(minDim: Int = 1, maxDim: Int = 25): Gen[pma.Matrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        def mmGen(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.Matrix)] = Gen.zip(matrixGen(minDim, maxDim), matrixGen(minDim, maxDim))
        def cvcvGen(minLength: Int = 1, maxLength: Int = 25): Gen[(pma.ColumnVector, pma.ColumnVector)] = Gen.zip(columnVectorGen(minLength, maxLength), columnVectorGen(minLength, maxLength))
        def rvrvGen(minLength: Int = 1, maxLength: Int = 25): Gen[(pma.RowVector, pma.RowVector)] = Gen.zip(rowVectorGen(minLength, maxLength), rowVectorGen(minLength, maxLength))
        def ssGen: Gen[(pma.Scalar, pma.Scalar)] = Gen.zip(sGen, sGen)
        
        "one to one" should {
            "support for S2S" in {
                def f(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = s
                def realDf(s: pma.Scalar): pma.Scalar = pma.one
                val df = d(f)(pma)
                forAll(sGen) { (s) =>
                    df(s) should be (realDf(s))
                }
            }
            "support for S2CV" in {
                def f(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.ColumnVector = alg.createColumnVectorFromElements(Seq(s, s, alg.zeroScalar))
                def realDf(s: pma.Scalar) = pma.createColumnVectorFromElements(Seq(pma.one, pma.one, pma.zeroScalar))
                val df = d(f)(pma)
                forAll(sGen) { (s) =>
                    df(s) should be (realDf(s))
                }
            }
            "support for S2RV" in {
                def f(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.RowVector = alg.createRowVectorFromElements(Seq(s, s, alg.zeroScalar))
                def realDf(s: pma.Scalar) = pma.createRowVectorFromElements(Seq(pma.one, pma.one, pma.zeroScalar))
                val df = d(f)(pma)
                forAll(sGen) { (s) =>
                    df(s) should be (realDf(s))
                }
            }
            "support for S2M" in {
                def f(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Matrix = alg.createMatrixFromElements(2, 3, Seq(s, s, alg.zeroScalar, s, alg.zeroScalar, s))
                def realDf(s: pma.Scalar) = pma.createMatrixFromElements(2, 3, Seq(pma.one, pma.one, pma.zeroScalar, pma.one, pma.zeroScalar, pma.one))
                val df = d(f)(pma)
                forAll(sGen) { (s) =>
                    df(s) should be (realDf(s))
                }
            }
            "support for CV2S" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = cv.elementAt(0)
                def realDf(cv: pma.ColumnVector): pma.ColumnVector = pma.zeroColumnVector(cv.length).setElementAt(0, pma.one)
                val df = d(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for CV2CV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.ColumnVector = cv
                def realDf(cv: pma.ColumnVector): pma.Matrix = eyeM(cv.length)
                val df = d(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for CV2CV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.ColumnVector = 
                    cv.setElementAt(0, cv.elementAt(0) + cv.elementAt(1)) // add non diagonal relation
                def realDf(cv: pma.ColumnVector): pma.Matrix = 
                    eyeM(cv.length).setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(columnVectorGen(minLength=2)) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for CV2RV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.RowVector = cv.t
                def realDf(cv: pma.ColumnVector): pma.Matrix = eyeM(cv.length)
                val df = d(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for CV2RV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.RowVector = { 
                    val res = cv.t 
                    res.setElementAt(0, res.elementAt(0) + cv.elementAt(1)) // add non diagonal relation
                }
                def realDf(cv: pma.ColumnVector): pma.Matrix = 
                    eyeM(cv.length)
                        .setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(columnVectorGen(minLength = 2, maxLength = 2)) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for CV2M" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Matrix = alg.stackColumns(Seq(cv, cv, cv))
                def realDf(cv: pma.ColumnVector): pma.Matrix = verticalStackMatrices(List(eyeM(cv.length), eyeM(cv.length), eyeM(cv.length)))
                val df = d(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    df(cv) should be (realDf(cv))
                }
            }
            "support for RV2S" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.Scalar = rv.elementAt(0)
                def realDf(rv: pma.RowVector): pma.RowVector = pma.zeroRowVector(rv.length).setElementAt(0, pma.one)
                val df = d(f)(pma)
                forAll(rowVectorGen()) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for RV2CV" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.ColumnVector = rv.t
                def realDf(rv: pma.RowVector): pma.Matrix = eyeM(rv.length)
                val df = d(f)(pma)
                forAll(rowVectorGen()) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for RV2CV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.ColumnVector = { 
                    val res = rv.t
                    res.setElementAt(0, res.elementAt(0) + rv.elementAt(1)) // add non diagonal relation 
                }
                def realDf(rv: pma.RowVector): pma.Matrix = 
                    import pma.*;
                    eyeM(rv.length)
                        .setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(rowVectorGen(minLength=2)) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for RV2RV" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.RowVector = rv
                def realDf(rv: pma.RowVector): pma.Matrix = eyeM(rv.length)
                val df = d(f)(pma)
                forAll(rowVectorGen()) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for RV2RV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.RowVector = 
                    val res = rv
                    res.setElementAt(0, res.elementAt(0) + rv.elementAt(1)) // add non diagonal relation
                def realDf(rv: pma.RowVector): pma.Matrix = 
                    import pma.*;
                    eyeM(rv.length)
                        .setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(rowVectorGen(minLength=2)) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for RV2M" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector): alg.Matrix = 
                    alg.stackRows(Seq(rv, rv, rv))
                def realDf(rv: pma.RowVector): pma.Matrix = 
                    pma.createMatrixFromElements(rv.length, rv.length * 3, 
                        for {
                            colJ <- 0 until rv.length * 3
                            rowI <- 0 until rv.length
                        } yield if rowI == Math.floor(colJ / 3) then pma.one else pma.zeroScalar
                    )
                val df = d(f)(pma)
                forAll(rowVectorGen()) { (rv) =>
                    df(rv) should be (realDf(rv))
                }
            }
            "support for M2S" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = m.elementAt(0, 0)
                def realDf(m: pma.Matrix): pma.Matrix = pma.zeroMatrix(m.nRows, m.nCols).setElementAt(0, 0, pma.one)
                val df = d(f)(pma)
                forAll(matrixGen()) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2CV" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.ColumnVector = m.columnAt(0)
                def realDf(m: pma.Matrix): pma.Matrix = eyeM2(m.nRows * m.nCols, m.nRows)
                val df = d(f)(pma)
                forAll(matrixGen()) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2CV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.ColumnVector = 
                    m   .columnAt(0)
                        .setElementAt(0, m.elementAt(0, 0) + m.elementAt(1, 0)) // add non diagonal relation 
                def realDf(m: pma.Matrix): pma.Matrix = 
                    eyeM2(m.nRows * m.nCols, m.nRows)
                        .setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(matrixGen(minDim=2)) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2RV" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.RowVector = { m.rowAt(0) }
                def realDf(m: pma.Matrix): pma.Matrix = eyeM2(m.nRows * m.nCols, m.nCols)
                val df = d(f)(pma)
                forAll(matrixGen(maxDim=2)) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2RV (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.RowVector = { 
                    
                    m   .rowAt(0)
                        .setElementAt(0, m.elementAt(0, 0) + m.elementAt(0, 1)) // add non diagonal relation 
                }
                def realDf(m: pma.Matrix): pma.Matrix = 
                    eyeM2(m.nRows * m.nCols, m.nCols)
                        .setElementAt(1, 0, pma.one) // add non diagonal relation to solution
                val df = d(f)(pma)
                forAll(matrixGen(minDim=2, maxDim=2)) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2M" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Matrix = m
                def realDf(m: pma.Matrix): pma.Matrix = eyeM(m.nRows * m.nCols)
                val df = d(f)(pma)
                forAll(matrixGen()) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
            "support for M2M (non-diagonal)" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Matrix =
                    m.setElementAt(0, 0, m.elementAt(0, 0) + m.elementAt(1, 0))
                def realDf(m: pma.Matrix): pma.Matrix = 
                    eyeM(m.nRows * m.nCols)
                        .setElementAt(1, 0, pma.one)
                val df = d(f)(pma)
                forAll(matrixGen(minDim=2, maxDim=2)) { (m) =>
                    df(m) should be (realDf(m))
                }
            }
        }
        "two to two" should {
            "support for SS2SS" in {
                def f(alg: MatrixAlgebraDSL)(s1: alg.Scalar, s2: alg.Scalar): (alg.Scalar, alg.Scalar) = (s1, s2)
                def realDf(s1: pma.Scalar, s2: pma.Scalar) = ((pma.one, pma.zeroScalar), (pma.zeroScalar, pma.one))
                val df = d(f)(pma)
                forAll(ssGen) { (s1, s2) =>
                    df(s1, s2) should be(realDf(s1, s2))
                }
            }
            "support for SM2MM" in {
                def f(alg: MatrixAlgebraDSL)(s: alg.Scalar, m: alg.Matrix): (alg.Scalar, alg.Matrix) = (s, m)
                def realDf(s: pma.Scalar, m: pma.Matrix) = 
                    ((pma.one, pma.zeroMatrix(m.nRows, m.nCols)), (pma.zeroMatrix(m.nRows, m.nCols), eyeM2(m.nRows * m.nCols, m.nRows * m.nCols)))
                val df = d(f)(pma)
                forAll(Gen.zip(sGen, matrixGen(maxDim=2))) { (s, m) =>
                    df(s, m) should be(realDf(s, m))
                }
            }
            "support for CVCV2CVCV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): (alg.ColumnVector, alg.ColumnVector) = (cv1, cv2)
                def realDf(cv1: pma.ColumnVector, cv2: pma.ColumnVector) = 
                    ((eyeM(cv1.length), pma.zeroMatrix(cv1.length, cv2.length)), (pma.zeroMatrix(cv2.length, cv1.length), eyeM(cv2.length)))
                val df = d(f)(pma)
                forAll(cvcvGen()) { (cv1, cv2) =>
                    df(cv1, cv2) should be(realDf(cv1, cv2))
                }
            }
            "support for CVM2CVM" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, m: alg.Matrix): (alg.ColumnVector, alg.Matrix) = (cv, m)
                def realDf(cv: pma.ColumnVector, m: pma.Matrix) = 
                    val numberOfInputsCV = cv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsCV = cv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM2(numberOfInputsCV, numberOfOutputsCV), pma.zeroMatrix(numberOfInputsCV, numberOfOutputsM)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsCV), eyeM2(numberOfInputsM, numberOfOutputsM)))

                val df = d(f)(pma)
                forAll(Gen.zip(columnVectorGen(), matrixGen())) { (cv, m) =>
                    df(cv, m) should be(realDf(cv, m))
                }
            }
            "support for RVRV2RVRV" in {
                def f(alg: MatrixAlgebraDSL)(rv1: alg.RowVector, rv2: alg.RowVector): (alg.RowVector, alg.RowVector) = (rv1, rv2)
                def realDf(rv1: pma.RowVector, rv2: pma.RowVector) = ((eyeM(rv1.length), pma.zeroMatrix(rv1.length, rv2.length)), (pma.zeroMatrix(rv2.length, rv1.length), eyeM(rv2.length)))
                val df = d(f)(pma)
                forAll(rvrvGen()) { (rv1, rv2) =>
                    df(rv1, rv2) should be(realDf(rv1, rv2))
                }
            }
            "support for RVM2RVM" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector, m: alg.Matrix): (alg.RowVector, alg.Matrix) = (rv, m)
                def realDf(rv: pma.RowVector, m: pma.Matrix) = 
                    val numberOfInputsRV = rv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsRV = rv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM2(numberOfInputsRV, numberOfOutputsRV), pma.zeroMatrix(numberOfInputsRV, numberOfOutputsM)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV), eyeM2(numberOfInputsM, numberOfOutputsM)))

                val df = d(f)(pma)
                forAll(Gen.zip(rowVectorGen(), matrixGen())) { (rv, m) =>
                    df(rv, m) should be(realDf(rv, m))
                }
            }
            "support for RVM2RVRV" in {
                def f(alg: MatrixAlgebraDSL)(rv: alg.RowVector, m: alg.Matrix): (alg.RowVector, alg.RowVector) = (rv, rv)
                def realDf(rv: pma.RowVector, m: pma.Matrix) = 
                    val numberOfInputsRV = rv.length
                    val numberOfInputsM = m.nRows * m.nCols
                    val numberOfOutputsRV = rv.length
                    val numberOfOutputsM = m.nRows * m.nCols
                    ((eyeM(rv.length), eyeM(rv.length)), (pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV), pma.zeroMatrix(numberOfInputsM, numberOfOutputsRV)))
                val df = d(f)(pma)
                forAll(Gen.zip(rowVectorGen(), matrixGen())) { (rv, m) =>
                    df(rv, m) should be(realDf(rv, m))
                }
            }
            "support for MM2MM" in {
                def f(alg: MatrixAlgebraDSL)(m1: alg.Matrix, m2: alg.Matrix): (alg.Matrix, alg.Matrix) = (m1, m2)
                def realDf(m1: pma.Matrix, m2: pma.Matrix) = 
                    val numberOfInputsM1 = m1.nRows * m1.nCols
                    val numberOfInputsM2 = m2.nRows * m2.nCols
                    val numberOfOutputsM1 = m1.nRows * m1.nCols
                    val numberOfOutputsM2 = m2.nRows * m2.nCols
                    ((eyeM2(numberOfInputsM1, numberOfOutputsM1), pma.zeroMatrix(numberOfInputsM1, numberOfOutputsM2)), (pma.zeroMatrix(numberOfInputsM2, numberOfOutputsM1), eyeM2(numberOfInputsM2, numberOfOutputsM2)))

                val df = d(f)(pma)
                forAll(mmGen(maxDim=2)) { (m1, m2) =>
                    df(m1, m2) should be(realDf(m1, m2))
                }
            }
        }
    }

