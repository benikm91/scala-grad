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
import scalagrad.api.ModeO
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL



trait AccessSetOpsTestSuit(
    val deriver: ModeO,
    val pma: MatrixAlgebraDSL,
    globalTestSuitParams: GlobalTestSuitParams[pma.Scalar, pma.ColumnVector, pma.RowVector, pma.Matrix],
) extends AnyWordSpec with BaseTestSuit[pma.Scalar, pma.ColumnVector, pma.RowVector, pma.Matrix]:

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    import globalTestSuitParams.*

    f"${testName} Matrix access operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25): Gen[pma.Matrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m

        "support setElementAtM" in {
            def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                val s = m.elementAt(0, 0)
                m.setElementAt(0, 0, s + s).sum
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

