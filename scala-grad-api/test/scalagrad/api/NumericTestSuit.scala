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

import scalagrad.api.matrixalgebra.MatrixAlgebra
import spire.math.Numeric

trait NumericTestSuit[
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
    deriverS: (DualScalar => DualScalar) => (PScalar => PScalar),
)(using 
    Numeric[PScalar],
    Numeric[DualScalar]
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra
    import deriverNumericalPlan.given

    f"${testName} Scalar" should {

        "support numeric operations" should {
            
            def testCase[S](testF: [S] => S => Numeric[S] ?=> S, sGen: Gen[PScalar] = sGen) = 
                {
                    // test function testF alone
                    def plainTestCase[S: Numeric, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s: S): S = testF[S](s)
                    val df = deriverS(plainTestCase[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                    val dfApprox = ScalaGrad.derive(plainTestCase[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                    forAll(sGen) { (s) =>
                        compareElementsSS(df(s), dfApprox(s))
                    }
                }
                {
                    // test function testF in a chain of operations
                    def chainedTestCase[S: Numeric, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s: S): S = 
                        import algebra.*
                        testF[S](s * s) / s
                    val df = deriverS(chainedTestCase[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                    val dfApprox = ScalaGrad.derive(chainedTestCase[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
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

