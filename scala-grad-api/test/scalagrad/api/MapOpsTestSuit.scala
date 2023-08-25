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
import scala.math.Fractional.Implicits.given
import spire.algebra.Trig
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.ModeO

trait MapOpsTestSuit extends AnyWordSpec:
    this: BaseTestSuit =>

    import scalagrad.numerical.NumericalForwardMode.{derive => dApprox}

    import deriver.{derive => d}

    f"${testName} Matrix map operations" should {

        def matrixGen(minDim: Int = 1, maxDim: Int = 25, mGen: (Gen[Int], Gen[Int]) => Gen[pma.Matrix] = mGen): Gen[pma.Matrix] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
            } yield m
        
        "mapElements" should {

            "support identify" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapElements(x => x).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support fractional square" in {
                def square[S: Fractional](s: S) = s * s
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapElements(square).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support fractional relu" in {
                import spire.compat.fractional
                def relu[S](s: S)(using frac: Fractional[S]) = 
                    import frac.*
                    if (s <= frac.zero) frac.zero
                    else s
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapElements(relu).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support spire.Trig log" in {
                def log[S](s: S)(using trig: Trig[S]) = 
                    trig.log(s)
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapElements(alg.trig.log).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen(mGen = positiveOnlyMGen)) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support complex operation" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    val m2 = m *:* m // do something before
                    val m2Sum = m2.elements.reduce(_ + _)
                    (
                        m2
                            .mapElements(x =>
                                // complex operation in the middle
                                x / m2Sum
                            ) 
                        // *:* m // do something after
                    ).sum
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
    
        "mapRows" should {
            "support identify" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapRows(x => x).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support basic operation" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    m.mapRows(x => x *:* x).sum
                val df = d(f)(pma)
                val dfApprox = dApprox(f)(pma)
                forAll(matrixGen()) { (m) =>
                    compareElementsMM(
                        df(m),
                        dfApprox(m)
                    )
                }
            }
            "support complex operation" in {
                def f(alg: MatrixAlgebraDSL)(m: alg.Matrix): alg.Scalar = 
                    (
                        (m *:* m) // do something before
                            .mapRows(x =>
                                // complex operation in the middle
                                val rowSum = x.sum
                                x / rowSum
                            ) 
                        *:* m // do something after
                    ).sum
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
    }

