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
import spire.algebra.Trig

trait TrigTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox

    f"${testName} Scalar" should {

        "support trig operations" should {
            
            def testCase[S](testF: [S] => S => Trig[S] ?=> S, sGen: Gen[pma.Scalar] = sGen) = 
                {
                    // test function testF alone
                    def plainTestCase(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = testF[alg.Scalar](s)
                    val df = d(plainTestCase)(pma)
                    val dfApprox = d(plainTestCase)(pma)
                    forAll(sGen) { (s) =>
                        compareElementsSS(
                            df(s),
                            dfApprox(s)
                        )
                    }
                }
                {
                    // test function testF in a chain of operations
                    def chainedTestCase(alg: MatrixAlgebraDSL)(s: alg.Scalar): alg.Scalar = testF[alg.Scalar](s * s) / s
                    val df = d(chainedTestCase)(pma)
                    val dfApprox = d(chainedTestCase)(pma)
                    forAll(sGen) { (s) =>
                        compareElementsSS(
                            df(s),
                            dfApprox(s)
                        )
                    }
                }
            
            "exp" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.exp(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "expm1" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.expm1(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "log" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.log(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = positiveOnlySGen)
            }
            "log1p" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.log1p(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = positiveOnlySGen)
            }
            "sin" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.sin(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            }
            "cos" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.cos(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            }
            "tan" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.tan(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "asin" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.asin(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "acos" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.acos(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "atan" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.atan(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            }
            // "atan2" in {
            //     def f[S](s: S)(using trig: Trig[S]) = trig.atan2(s)
            //     testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            // }
            "sinh" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.sinh(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "cosh" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.cosh(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "tanh" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.tanh(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x), sGen = smallSGen)
            }
            "toRadians" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.toRadians(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            }
            "toDegrees" in {
                def f[S](s: S)(using trig: Trig[S]) = trig.toDegrees(s)
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
            }
        }
    }

