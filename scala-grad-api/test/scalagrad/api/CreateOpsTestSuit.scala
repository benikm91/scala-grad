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

import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scala.math.Fractional.Implicits.given
import spire.algebra.Trig

trait CreateOpsTestSuit[
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
    deriverM: (DualMatrix => DualScalar) => (PMatrix => PMatrix),
    deriverCV: (DualColumnVector => DualScalar) => (PColumnVector => PColumnVector),
    deriverS: (DualScalar => DualScalar) => (PScalar => PScalar),
)(using 
    Fractional[PScalar],
    Fractional[DualScalar],
    Trig[PScalar],
    Trig[DualScalar]
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra

    import dualAlgebra.*
    import primaryAlgebra.*
    import deriverNumericalPlan.given

    f"${testName} Matrix create operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25, mGen: (Gen[Int], Gen[Int]) => Gen[PMatrix] = mGen): Gen[PMatrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m
        
        "stackColumns" should {
            "support identify" in {
                def f[S: Fractional, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                    import algebra.*
                    algebra.stackColumns(m.columns).sum
                val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
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
                def f[S: Fractional, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                    import algebra.*
                    algebra.stackRows(m.rows).sum
                val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }
    }

