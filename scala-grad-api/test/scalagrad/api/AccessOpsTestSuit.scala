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
) extends AnyWordSpec with BaseTestSuit[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    import globalTestSuitParams.*
    override val primaryAlgebra = globalTestSuitParams.primaryAlgebra

    import dualAlgebra.*
    import primaryAlgebra.*
    import deriverNumericalPlan.given

    f"${testName} Matrix access operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25): Gen[PMatrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        "support elementAtM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                m.elementAt(0, 0)
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support elementAtM with operation" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                m.elementAt(0, 0) * m.elementAt(1, 0) * m.elementAt(0, 1)
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support columnAtM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                m.columnAt(0).sum
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support columnAtM with operations" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                (m.columnAt(0) *:* m.columnAt(1)).sum
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support rowAtM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                m.rowAt(0).sum
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen()) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
        "support rowAtM with operations" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M): S = 
                import algebra.*
                (m.rowAt(0) *:* m.rowAt(1)).sum
            val df = deriverM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(matrixGen(minDim = 2)) { (m) =>
                compareElementsMM(
                    df(m),
                    dfApprox(m)
                )
            }
        }
    }

    f"${testName} Vector access operations" should {

        def columnVectorGen(minLength: Int = 1, maxLength: Int = 25): Gen[PColumnVector] =
            for {
                length <- Gen.choose(minLength, maxLength)
                cv <- cvGen(length)
            } yield cv

        f"ColumnVector operations" should {
            "support elementAtCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV): S = 
                    import algebra.*
                    cv.elementAt(0)
                val df = deriverCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(columnVectorGen()) { (cv) =>
                    compareElementsCVCV(
                        df(cv),
                        dfApprox(cv)
                    )
                }
            }
            "support elementAtCV with operation" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV): S = 
                    import algebra.*
                    cv.elementAt(0) * cv.elementAt(1)
                val df = deriverCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(columnVectorGen(minLength = 2)) { (m) =>
                    compareElementsCVCV(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }

        f"RowVector operations" should {
            "support elementAtRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV): S = 
                    import algebra.*
                    cv.t.elementAt(0)
                val df = deriverCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(columnVectorGen()) { (cv) =>
                    compareElementsCVCV(
                        df(cv),
                        dfApprox(cv)
                    )
                }
            }
            "support elementAtRV with operation" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV): S = 
                    import algebra.*
                    cv.t.elementAt(0) * cv.t.elementAt(1)
                val df = deriverCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(columnVectorGen(minLength = 2)) { (m) =>
                    compareElementsCVCV(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
        }
    }

