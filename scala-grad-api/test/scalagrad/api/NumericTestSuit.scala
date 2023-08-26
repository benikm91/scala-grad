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
import spire.math.Numeric
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL

trait NumericTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    f"${testName} Scalar" should {

        "support numeric operations" should {
            
            def testCase[S](testF: [S] => S => Numeric[S] ?=> S, sGen: Gen[pma.Scalar] = sGen) = 
                {
                    // test function testF alone
                    def plainTestCase(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = testF[alg.Scalar](s)
                    val df = d(plainTestCase)(pma)
                    val dfApprox = dApprox(plainTestCase)(pma)
                    forAll(sGen) { (s) =>
                        compareElementsSS(df(s), dfApprox(s))
                    }
                }
                {
                    // test function testF in a chain of operations
                    def chainedTestCase(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = testF[alg.Scalar](s * s) / s
                    val df = d(chainedTestCase)(pma)
                    val dfApprox = dApprox(chainedTestCase)(pma)
                    forAll(sGen) { (s) =>
                        compareElementsSS(df(s), dfApprox(s))
                    }
                }
            
            "ceil" in {
                def f[S](s: S)(using num: Numeric[S]) = num.ceil(s)
                testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](x))
            }
            "floor" in {
                def f[S](s: S)(using num: Numeric[S]) = num.floor(s)
                testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](x))
            }
            "round" in {
                def f[S](s: S)(using num: Numeric[S]) = num.round(s)
                testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](x))
            }
            // "fpow" in {
            //     def f[S](s: S)(using num: Numeric[S]) = num.fpow(s)
            //     testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](x), sGen = smallSGen)
            // }
            "nroot" in {
                def f[S](n: Int)(s: S)(using num: Numeric[S]) = num.nroot(s, n)
                forAll(Gen.choose(2, 10)) { (n) => 
                    testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](n)(x), sGen = positiveOnlySGen)
                }
            }
            "abs" in {
                def f[S](s: S)(using num: Numeric[S]) = num.abs(s)
                testCase([S] => (x: S) => (num: Numeric[S]) ?=> f[S](x))
            }
        }
    }

