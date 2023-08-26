package scalagrad.api.test

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
import scalagrad.api.forward.ForwardMode
import scalagrad.api.reverse.ReverseMode
import breeze.linalg.{DenseMatrix}

trait AccessOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    f"${testName} Matrix access operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25): Gen[pma.Matrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        "support elementAtM" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.elementAt(0, 0)
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support elementAtM with operation" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.elementAt(0, 0) * m.elementAt(1, 0) * m.elementAt(0, 1)
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support columnAtM" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.columnAt(0).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support columnAtM with operations" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                (m.columnAt(0) *:* m.columnAt(1)).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support rowAtM" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.rowAt(0).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support rowAtM with operations" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                (m.rowAt(0) *:* m.rowAt(1)).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
    }

    f"${testName} Vector access operations" should {

        def columnVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[pma.ColumnVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                cv <- cvGen(length)
            } yield cv

        f"ColumnVector operations" should {
            "support elementAtCV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = 
                    import algebra.*
                    cv.elementAt(0)
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    compareElementsCVCV(
                        df(cv),
                        dfApprox(cv)
                    )
                }
            }
            "support elementAtCV with operation" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = 
                    import algebra.*
                    cv.elementAt(0) * cv.elementAt(1)
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(columnVectorGen(minLength = 2)) { (m) =>
                    compareElementsCVCV(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }

        f"RowVector operations" should {
            "support elementAtRV" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = 
                    import algebra.*
                    cv.t.elementAt(0)
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(columnVectorGen()) { (cv) =>
                    compareElementsCVCV(
                        df(cv),
                        dfApprox(cv)
                    )
                }
            }
            "support elementAtRV with operation" in {
                def f(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = 
                    import algebra.*
                    cv.t.elementAt(0) * cv.t.elementAt(1)
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(columnVectorGen(minLength = 2)) { (m) =>
                    compareElementsCVCV(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }
    }

