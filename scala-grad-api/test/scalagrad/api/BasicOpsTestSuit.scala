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

trait BasicOpsTestSuit[
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
    deriverMM: ((DualMatrix, DualMatrix) => DualScalar) => (Tuple2[PMatrix, PMatrix] => (PMatrix, PMatrix)),
    deriverMCV: ((DualMatrix, DualColumnVector) => DualScalar) => (Tuple2[PMatrix, PColumnVector] => (PMatrix, PColumnVector)),
    deriverMRV: ((DualMatrix, DualRowVector) => DualScalar) => (Tuple2[PMatrix, PRowVector] => (PMatrix, PRowVector)),
    deriverMS: ((DualMatrix, DualScalar) => DualScalar) => (Tuple2[PMatrix, PScalar] => (PMatrix, PScalar)),
    deriverCVCV: ((DualColumnVector, DualColumnVector) => DualScalar) => (Tuple2[PColumnVector, PColumnVector] => (PColumnVector, PColumnVector)),
    deriverCVS: ((DualColumnVector, DualScalar) => DualScalar) => (Tuple2[PColumnVector, PScalar] => (PColumnVector, PScalar)),
    deriverSS: ((DualScalar, DualScalar) => DualScalar) => (Tuple2[PScalar, PScalar] => (PScalar, PScalar)),
    tolerance: PScalar = 1,
) extends AnyWordSpec with should.Matchers:
    
    import globalTestSuitParams.*
    import dualAlgebra.*
    import primaryAlgebra.*
    import deriverNumericalPlan.given

    def compareAllElementsMS(t1: (PMatrix, PScalar), t2: (PMatrix, PScalar)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsMRV(t1: (PMatrix, PRowVector), t2: (PMatrix, PRowVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsRVRV(t1._2, t2._2)

    def compareAllElementsMCV(t1: (PMatrix, PColumnVector), t2: (PMatrix, PColumnVector)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsMM(t1: (PMatrix, PMatrix), t2: (PMatrix, PMatrix)) = 
        compareElementsMM(t1._1, t2._1)
        compareElementsMM(t1._2, t2._2)

    def compareAllElementsCVCV(t1: (PColumnVector, PColumnVector), t2: (PColumnVector, PColumnVector)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsCVCV(t1._2, t2._2)

    def compareAllElementsCVS(t1: (PColumnVector, PScalar), t2: (PColumnVector, PScalar)) = 
        compareElementsCVCV(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

    def compareAllElementsSS(t1: (PScalar, PScalar), t2: (PScalar, PScalar)) = 
        compareElementsSS(t1._1, t2._1)
        compareElementsSS(t1._2, t2._2)

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

    f"${testName} (Matrix, Matrix) operations" should {

        def genSameDimMM(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PMatrix)] =
            for {
                nRow <- Gen.choose(minDim, maxDim) 
                nCol <- Gen.choose(minDim, maxDim) 
                m1 <- mGen(nRow, nCol)
                m2 <- mGen(nRow, nCol)
            } yield (m1, m2)
            
        "support dotMM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m1: M, m2: M): S = 
                import algebra.*
                (m1 * m2).sum
            val df = deriverMM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            val dimGen = Gen.choose(1, 25)
            val pariMGen = for {
                nRow <- dimGen
                nCol <- dimGen
                mCol <- dimGen
                m1 <- mGen(nRow, nCol)
                m2 <- mGen(nCol, mCol)
            } yield (m1, m2)
            forAll(pariMGen) { (m1, m2) =>
                compareAllElementsMM(
                    df(m1, m2),
                    dfApprox(m1, m2)
                )
            }
        }
        "support plusMM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m1: M, m2: M): S = 
                import algebra.*
                (m1 + m2).sum
            val df = deriverMM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
        "support minusMM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m1: M, m2: M): S = 
                import algebra.*
                (m1 - m2).sum
            val df = deriverMM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
        "support elementWiseMM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m1: M, m2: M): S = 
                import algebra.*
                (m1 *:* m2).sum
            val df = deriverMM(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSameDimMM()) { (m1, m2) =>
                compareAllElementsMM(df(m1, m2), dfApprox(m1, m2))
            }
        }
    }
    f"${testName} (Matrix, ColumnVector) operations" should {

        def genMCV(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PColumnVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                cv <- cvGen(nCol)
            } yield (m, cv)

        def genMCV2(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PColumnVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                cv <- cvGen(nRow)
            } yield (m, cv)

        "support dotMCV" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, cv: CV): S = 
                import algebra.*
                (m * cv).sum
            val df = deriverMCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            val dimGen = Gen.choose(1, 25)
            forAll(genMCV()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
        "support plusMCV" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, cv: CV): S = 
                import algebra.*
                (m + cv).sum
            val df = deriverMCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMCV2()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
        "support minusMCV" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, cv: CV): S = 
                import algebra.*
                (m - cv).sum
            val df = deriverMCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMCV2()) { (m, cv) =>
                compareAllElementsMCV(
                    df(m, cv),
                    dfApprox(m, cv)
                )
            }
        }
    }
    f"${testName} (Matrix, RowVector) operations" should {

        def genMRV(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PRowVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                rv <- rvGen(nRow)
            } yield (m, rv)

        def genMRV2(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PRowVector)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                rv <- rvGen(nCol)
            } yield (m, rv)

        "support dotRVM" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, rv: RV): S = 
                import algebra.*
                (rv * m).sum
            val df = deriverMRV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMRV()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
        "support plusMRV" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, rv: RV): S = 
                import algebra.*
                (m + rv).sum
            val df = deriverMRV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMRV2()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
        "support minusMRV" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, rv: RV): S = 
                import algebra.*
                (m - rv).sum
            val df = deriverMRV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMRV2()) { (m, rv) =>
                compareAllElementsMRV(
                    df(m, rv),
                    dfApprox(m, rv)
                )
            }
        }
    }
    f"${testName} (Matrix, Scalar) operations" should {

        def genMS(minDim: Int = 1, maxDim: Int = 25): Gen[(PMatrix, PScalar)] =
            for {
                nRow <- Gen.choose(minDim, maxDim)
                nCol <- Gen.choose(minDim, maxDim)
                m <- mGen(nRow, nCol)
                s <- sGen
            } yield (m, s)

        "support multiplyMS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, s: S): S = 
                import algebra.*
                (m * s).sum
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support plusMS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, s: S): S = 
                import algebra.*
                (m + s).sum
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support minusMS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, s: S): S = 
                import algebra.*
                (m - s).sum
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
        "support divideMS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(m: M, s: S): S = 
                import algebra.*
                (m / s).sum
            val df = deriverMS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genMS()) { (m, s) =>
                compareAllElementsMS(
                    df(m, s),
                    dfApprox(m, s)
                )
            }
        }
    }

    f"${testName} (Vector, Vector) operations" should {
        def genCVCV(minDim: Int = 1, maxDim: Int = 25): Gen[(PColumnVector, PColumnVector)] =
            for {
                length <- Gen.choose(minDim, maxDim)
                cv1 <- cvGen(length)
                cv2 <- cvGen(length)
            } yield (cv1, cv2)
        "(ColumnVector, ColumnVector) operations" should {
            "support elementWiseMultiplyCVCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1 *:* cv2).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support plusCVCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1 + cv2).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support minusCVCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1 - cv2).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
        "(ColumnVector, RowVector) operations" should {
            "support dotCVRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1 * cv2.t).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support dotRVCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    cv1.t * cv2
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
        f"(RowVector, RowVector) operations" should {
            "support elementWiseMultiplyRVRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    sum(cv1.t *:* cv2.t)
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support plusRVRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1.t + cv2.t).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
            "support minusRVRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv1: CV, cv2: CV): S = 
                    import algebra.*
                    (cv1.t - cv2.t).sum
                val df = deriverCVCV(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVCV()) { (cv1, cv2) =>
                    compareAllElementsCVCV(
                        df(cv1, cv2),
                        dfApprox(cv1, cv2)
                    )
                }
            }
        }
    }
    f"${testName} (Vector, Scalar) operations" should {
        def genCVS(minDim: Int = 1, maxDim: Int = 25): Gen[(PColumnVector, PScalar)] =
            for {
                length <- Gen.choose(minDim, maxDim)
                cv <- cvGen(length)
                s <- sGen
            } yield (cv, s)
        f"(ColumnVector, Scalar) operations" should {
            "support multiplyCVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv * s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }
            }
            "support plusCVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv + s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support minusCV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv - s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support divideCVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv / s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }  
        }
        f"(RowVector, Scalar) operations" should {
            "support multiplyRVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv.t * s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }
            }
            "support plusRVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv.t + s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support minusRV" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv.t - s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }
            "support divideRVS" in {
                def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(cv: CV, s: S): S = 
                    import algebra.*
                    (cv.t / s).sum
                val df = deriverCVS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
                val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
                forAll(genCVS()) { (cv, s) =>
                    compareAllElementsCVS(
                        df(cv, s),
                        dfApprox(cv, s)
                    )
                }  
            }  
        }
    }
    f"${testName} (Scalar, Scalar) operations" should {
        def genSS: Gen[(PScalar, PScalar)] =
            for {
                s1 <- sGen
                s2 <- sGen
            } yield (s1, s2)
        "support multiplySS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s1: S, s2: S): S = 
                import algebra.*
                s1 * s2
            val df = deriverSS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }
        }
        "support plusSS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s1: S, s2: S): S = 
                import algebra.*
                s1 + s2
            val df = deriverSS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }    
        }
        "support minusSS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s1: S, s2: S): S = 
                import algebra.*
                s1 - s2
            val df = deriverSS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }    
        }
        "support divideSS" in {
            def f[S, CV, RV, M](algebra: MatrixAlgebra[S, CV, RV, M])(s1: S, s2: S): S = 
                import algebra.*
                s1 / s2
            val df = deriverSS(f[DualScalar, DualColumnVector, DualRowVector, DualMatrix](dualAlgebra))
            val dfApprox = ScalaGrad.derive(f[PScalar, PColumnVector, PRowVector, PMatrix](primaryAlgebra))
            forAll(genSS) { (s1, s2) =>
                compareAllElementsSS(
                    df(s1, s2),
                    dfApprox(s1, s2)
                )
            }
        }
    }

