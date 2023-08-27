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
import scalagrad.api.{Mode, dual}

trait CreateOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox

    f"${testName} Matrix create operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25, mGen: (Gen[Int], Gen[Int]) => Gen[pma.Matrix] = mGen): Gen[pma.Matrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m
        
        "stackColumns" should {
            "support identify" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    alg.stackColumns(m.columns).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }
        "stackRows" should {
            "support identify" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    alg.stackRows(m.rows).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }
    }

