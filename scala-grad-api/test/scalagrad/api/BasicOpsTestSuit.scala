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

trait BasicOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    f"${testName} (Matrix, Matrix) operations" should {

        def genSameDimMM(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.Matrix)] =
            for {
                nRow <- Gen.choose(minDim, maxDim) 
                nCol <- Gen.choose(minDim, maxDim) 
                m1 <- mGen(nRow, nCol)
                m2 <- mGen(nRow, nCol)
            } yield (m1, m2)
            
        "support dotMM" in {
            def f(alg: MatrixAlgebraDSL)(m1: alg.Matrix, m2: alg.Matrix): alg.Scalar = 
                (m1 * m2).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            val dimGen = Gen.choose(1, 25)
            val pariMGen = for {
                nRow <- dimGen
                nCol <- dimGen
                mCol <- dimGen
                m1 <- mGen(nRow, nCol)
                m2 <- mGen(nCol, mCol)
            } yield (m1, m2)
            forAll(pariMGen) { (m1, m2) =>
                compareAllElementsMM(
                    df(m1, m2),
                    dfApprox(m1, m2)
                )
            }
        }
        "support plusMM" in {
            def f(alg: MatrixAlgebraDSL)(m1: alg.Matrix, m2: alg.Matrix): alg.Scalar = 
                (m1 + m2).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
        "support minusMM" in {
            def f(alg: MatrixAlgebraDSL)(m1: alg.Matrix, m2: alg.Matrix): alg.Scalar = 
                (m1 - m2).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
        "support elementWiseMM" in {
            def f(alg: MatrixAlgebraDSL)(m1: alg.Matrix, m2: alg.Matrix): alg.Scalar = 
                (m1 *:* m2).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
    }
    f"${testName} (Matrix, ColumnVector) operations" should {

        def genMCV(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.ColumnVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                cv <- cvGen(nCol)
            } yield (m, cv)

        def genMCV2(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.ColumnVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                cv <- cvGen(nRow)
            } yield (m, cv)

        "support dotMCV" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, cv: alg.ColumnVector): alg.Scalar = 
                (m * cv).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            val dimGen = Gen.choose(1, 25)
            forAll(genMCV()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
        "support plusMCV" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, cv: alg.ColumnVector): alg.Scalar = 
                (m + cv).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMCV2()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
        "support minusMCV" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, cv: alg.ColumnVector): alg.Scalar = 
                (m - cv).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMCV2()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
    }
    f"${testName} (Matrix, RowVector) operations" should {

        def genMRV(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.RowVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                rv <- rvGen(nRow)
            } yield (m, rv)

        def genMRV2(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.RowVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                rv <- rvGen(nCol)
            } yield (m, rv)

        "support dotRVM" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, rv: alg.RowVector): alg.Scalar = 
                (rv * m).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMRV()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
        "support plusMRV" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, rv: alg.RowVector): alg.Scalar = 
                (m + rv).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMRV2()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
        "support minusMRV" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, rv: alg.RowVector): alg.Scalar = 
                (m - rv).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMRV2()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
    }
    f"${testName} (Matrix, Scalar) operations" should {

        def genMS(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.Matrix, pma.Scalar)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                s <- sGen
            } yield (m, s)

        "support multiplyMS" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, s: alg.Scalar): alg.Scalar = 
                (m * s).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support plusMS" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, s: alg.Scalar): alg.Scalar = 
                (m + s).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support minusMS" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, s: alg.Scalar): alg.Scalar = 
                (m - s).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support divideMS" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix, s: alg.Scalar): alg.Scalar = 
                (m / s).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
    }

    f"${testName} (Vector, Vector) operations" should {
        def genCVCV(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.ColumnVector, pma.ColumnVector)] =
            for {
                length <- Gen.choose(minDim, maxDim)
                cv1 <- cvGen(length)
                cv2 <- cvGen(length)
            } yield (cv1, cv2)
        "(ColumnVector, ColumnVector) operations" should {
            "support elementWiseMultiplyCVCV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1 *:* cv2).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support plusCVCV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1 + cv2).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support minusCVCV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1 - cv2).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
        "(ColumnVector, RowVector) operations" should {
            "support dotCVRV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1 * cv2.t).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support dotRVCV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    cv1.t * cv2
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
        f"(RowVector, RowVector) operations" should {
            "support elementWiseMultiplyRVRV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1.t *:* cv2.t).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support plusRVRV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1.t + cv2.t).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support minusRVRV" in {
                def f(alg: MatrixAlgebraDSL)(cv1: alg.ColumnVector, cv2: alg.ColumnVector): alg.Scalar = 
                    (cv1.t - cv2.t).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
    }
    f"${testName} (Vector, Scalar) operations" should {
        def genCVS(minDim: Int = 1, maxDim: Int = 25): Gen[(pma.ColumnVector, pma.Scalar)] =
            for {
                length <- Gen.choose(minDim, maxDim)
                cv <- cvGen(length)
                s <- sGen
            } yield (cv, s)
        f"(ColumnVector, Scalar) operations" should {
            "support multiplyCVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv * s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }
            }
            "support plusCVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv + s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support minusCV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv - s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support divideCVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv / s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }  
        }
        f"(RowVector, Scalar) operations" should {
            "support multiplyRVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv.t * s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }
            }
            "support plusRVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv.t + s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support minusRV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv.t - s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support divideRVS" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, s: alg.Scalar): alg.Scalar = 
                    (cv.t / s).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }  
        }
    }
    f"${testName} (Scalar, Scalar) operations" should {
        def genSS: Gen[(pma.Scalar, pma.Scalar)] =
            for {
                s1 <- sGen
                s2 <- sGen
            } yield (s1, s2)
        "support multiplySS" in {
            def f(alg: MatrixAlgebraDSL)(s1: alg.Scalar, s2: alg.Scalar): alg.Scalar = 
                s1 * s2
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }
        }
        "support plusSS" in {
            def f(alg: MatrixAlgebraDSL)(s1: alg.Scalar, s2: alg.Scalar): alg.Scalar = 
                s1 + s2
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }    
        }
        "support minusSS" in {
            def f(alg: MatrixAlgebraDSL)(s1: alg.Scalar, s2: alg.Scalar): alg.Scalar = 
                s1 - s2
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }    
        }
        "support divideSS" in {
            def f(alg: MatrixAlgebraDSL)(s1: alg.Scalar, s2: alg.Scalar): alg.Scalar = 
                s1 / s2
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }
        }
    }

