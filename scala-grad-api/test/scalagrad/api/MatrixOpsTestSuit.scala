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
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import scalagrad.numerical.DeriverNumericalPlan
import scalagrad.api.forward.ForwardMode
import scalagrad.api.reverse.ReverseMode

import scalagrad.api.matrixalgebra.MatrixAlgebra
import spire.math.Numeric

trait MatrixOpsTestSuit[
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
    deriverMS: (DualMatrix => DualScalar) => (PMatrix => PMatrix),
)(using 
    Numeric[PScalar],
    Numeric[DualScalar]
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra
    import deriverNumericalPlan.given

    def getSquareM(minDim: Int = 1, maxDim: Int = 25): Gen[PMatrix] =
        for {
            n <- Gen.choose(minDim, maxDim) 
            m <- mGen(n, n)
        } yield m
        
    f"${testName} Matrix" should {
        "support inverse" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                algebra.inverse(m).sum
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(getSquareM()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support determinant" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                algebra.determinant(m / algebra.liftToScalar(1_000))
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(getSquareM()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
    }

