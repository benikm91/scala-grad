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
import spire.algebra.Trig

trait TrigTestSuit[
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
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
    deriverS: (DualScalar => DualScalar) => (PScalar => PScalar),
)(using 
    Trig[PScalar],
    Trig[DualScalar]
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra
    import deriverNumericalPlan.given

    f"${testName} Scalar" should {

        "support trig operations" should {
            
            def testCase[S](testF: [S] => S => Trig[S] ?=> S, sGen: Gen[PScalar] = sGen) = 
                def plainTestCase[S: Trig, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s: S): S = testF[S](s)
                val df = deriverS(plainTestCase[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(plainTestCase[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(sGen) { (s) =>
                    compareElementsSS(
                        df(s),
                        dfApprox(s)
                    )
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
                testCase([S] => (x: S) => (trig: Trig[S]) ?=> f[S](x))
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

