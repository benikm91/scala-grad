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
import spire.math.Numeric

trait MatrixOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox

    def getSquareM(minDim: Int = 1, maxDim: Int = 25): Gen[pma.Matrix] =
        for {
            n <- Gen.choose(minDim, maxDim) 
            m <- mGen(n, n)
        } yield m
        
    f"${testName} Matrix" should {
        "support inverse" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                alg.inverse(m).sum
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(getSquareM()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support determinant" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                alg.determinant(m / alg.lift(1_000))
            val df = d(f)(pma)
            val dfApprox = dApprox(f)(pma)
            forAll(getSquareM()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
    }

