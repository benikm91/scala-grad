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


trait AccessSetOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox

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

