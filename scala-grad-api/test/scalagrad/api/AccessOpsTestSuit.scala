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



trait AccessOpsTestSuit[
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
    tolerance: PScalar = 1,
) extends AnyWordSpec with should.Matchers:
    
    import globalTestSuitParams.*
    import dualAlgebra.*
    import primaryAlgebra.*
    import deriverNumericalPlan.given

    def compareElementsSS(s1: PScalar, s2: PScalar) = 
        (s1: Double) should be((s2: Double) +- tolerance)

    def compareElementsCVCV(cv1: PColumnVector, cv2: PColumnVector) =
        cv1.length should be(cv2.length)
        cv1.toArray.zip(cv2.toArray).foreach { (e1, e2) =>
            e1 should be(e2 +- tolerance)
        }

    def compareElementsRVRV(rv1: PRowVector, rv2: PRowVector) = compareElementsCVCV(
        primaryAlgebra.transposeRowVector(rv1), 
        primaryAlgebra.transposeRowVector(rv2),
    )

    def compareElementsMM(m1: PMatrix, m2: PMatrix) = 
        m1.rows should be(m2.rows)
        m1.cols should be(m2.cols)
        m1.toArray.zip(m2.toArray).foreach { (e1, e2) =>
            e1 should be(e2 +- tolerance)
        }

    f"${testName} Matrix access operations" should {

        def mGen(minDim: Int = 1, maxDim: Int = 25): Gen[PMatrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        "support elementAtM" in {
            /*def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                m.elementAt(0, 0)
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(mGen) { m =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }*/
        }
    }

