package scalagrad.api.test

import org.scalacheck.Gen
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.{Mode, dual}
import spire.algebra.Trig

import scala.math.Fractional.Implicits.given

trait MapDualOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox
    
    f"${testName} Scalar mapDual operations" should {

        "support deleting gradients" in {
            def f(alg: DualMatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = 
                s.mapDual(x => x, x => alg.primaryMatrixAlgebra.zeroScalar)
            val df = d(f)(pma)
            forAll(sGen) { (s) =>
                compareElementsSS(
                    df(s),
                    // No gradients <=> gradients were deleted
                    pma.zeroScalar
                )
            }
        }
    }

    f"${testName} ColumnVector mapDual operations" should {

        def columnVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[pma.ColumnVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                cv <- cvGen(length)
            } yield cv

        "support deleting gradients" in {
            def f(alg: DualMatrixAlgebraDSL)(cv: alg.ColumnVector): alg.Scalar = 
                cv.mapDual(x => x, x => alg.primaryMatrixAlgebra.zeroScalar).sum
            val df = d(f)(pma)
            forAll(columnVectorGen()) { (cv) =>
                compareElementsCVCV(
                    df(cv),
                    // No gradients <=> gradients were deleted
                    pma.zeroColumnVector(cv.length)
                )
            }
        }
    }

    f"${testName} RowVector mapDual operations" should {

            def rowVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[pma.RowVector] =
                for {
                    length <- Gen.choose(minLength, maxLength)
                    rv <- rvGen(length)
                } yield rv

            "support deleting gradients" in {
                def f(alg: DualMatrixAlgebraDSL)(rv: alg.RowVector): alg.Scalar = 
                    rv.mapDual(x => x, x => alg.primaryMatrixAlgebra.zeroScalar).sum
                val df = d(f)(pma)
                forAll(rowVectorGen()) { (rv) =>
                    compareElementsRVRV(
                        df(rv),
                        // No gradients <=> gradients were deleted
                        pma.zeroRowVector(rv.length)
                    )
                }
            }
        }

    def matrixGen(minDim: Int = 1, maxDim: Int = 25, mGen: (Gen[Int], Gen[Int]) => Gen[pma.Matrix] = mGen): Gen[pma.Matrix] =
        for {
            nRow <- Gen.choose(minDim, maxDim)
            nCol <- Gen.choose(minDim, maxDim)
            m <- mGen(nRow, nCol)
        } yield m
    
    f"${testName} Matrix mapDual operations" should {
        "support deleting gradients" in {
            def f(alg: DualMatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.mapDual(x => x, x => alg.primaryMatrixAlgebra.zeroScalar).sum
            val df = d(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    // No gradients <=> gradients were deleted
                    pma.zeroMatrix(m.nRows, m.nCols)
                )
            }
        }
    }

    f"${testName} Matrix mapDualRows operations" should {
        "support deleting gradients" in {
            def f(alg: DualMatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.mapDualRows(x => x, x => alg.primaryMatrixAlgebra.zeroRowVector(alg.primaryMatrixAlgebra.lengthRowVector(x))).sum
            val df = d(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    // No gradients <=> gradients were deleted
                    pma.zeroMatrix(m.nRows, m.nCols)
                )
            }
        }
    }

    f"${testName} Matrix mapDualColumn operations" should {
        "support deleting gradients" in {
            def f(alg: DualMatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                m.mapDualColumns(x => x, x => alg.primaryMatrixAlgebra.zeroColumnVector(alg.primaryMatrixAlgebra.lengthColumnVector(x))).sum
            val df = d(f)(pma)
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    // No gradients <=> gradients were deleted
                    pma.zeroMatrix(m.nRows, m.nCols)
                )
            }
        }
    }
