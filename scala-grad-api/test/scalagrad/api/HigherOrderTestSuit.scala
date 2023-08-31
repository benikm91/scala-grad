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

trait HigherOrderTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import deriver.derive as d
    import scalagrad.numerical.NumericalForwardMode.derive as dApprox

    f"${testName} support higher-order derivatives" should {
        "can derive x^5" should {
            "support for S2S" in {
                def f(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
                    (1 to 5).map(_ => x).reduce(_ * _)
                def realDf(order: Int)(x: pma.Scalar): pma.Scalar = 
                    val scale = pma.lift((5-(order-1) to 5).reduce(_ * _))
                    scale * (order+1 to 5).map(_ => x).reduce(_ * _)
                val df = d(f)(pma)
                val ddf = d(d(f))(pma)
                val dddf = d(d(d(f)))(pma)
                val ddddf = d(d(d(d(f))))(pma)
                forAll(sGen) { bigX => 
                    val x = bigX / pma.lift(100)
                    compareElementsSS(df(x),    realDf(1)(x))
                    compareElementsSS(ddf(x),   realDf(2)(x))
                    compareElementsSS(dddf(x),  realDf(3)(x))
                    compareElementsSS(ddddf(x), realDf(4)(x))
                }
            }
        }
    }

