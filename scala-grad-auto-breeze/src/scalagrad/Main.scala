package scalagrad

import scalagrad.api.ScalaGrad
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scala.util.TupledFunction
import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan
import scalagrad.api.matrixalgebra.MatrixAlgebraT

def gWithTypeDependency(mat: MatrixAlgebraT)(
    x1: mat.Scalar,
    x2: mat.Scalar
): mat.Scalar = x1 * x2

def gWithTypeClass[Scalar, ColumnVector, RowVector, Matrix](
    x1: Scalar,
    x2: Scalar
)(using ma: MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix]): Scalar = x1 * x2

@main
def forwardMulti = 
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan.given
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan.algebra.*
    import scalagrad.api.forward.dual.*

    def fx(
        x1: DualNumberScalar[Double],
        x2: DualNumberScalar[Double],
        x3: DualNumberScalar[Double]
    ): (DualNumberScalar[Double], DualNumberScalar[Double]) = (x1, x2)

    val t = ScalaGrad.derive(fx.tupled)
    val tRes: ((Double, Double), (Double, Double), (Double, Double)) = t(1.0, 2.0, 3.0)
    println(tRes)

    def fx2(
        x1: DualNumberScalar[Double],
        x2: DualNumberColumnVector[DenseVector[Double]]
    ): (DualNumberScalar[Double], DualNumberScalar[Double]) = (x1, x1)

    val t2 = ScalaGrad.derive(fx2.tupled)
    val t2Res: ((Double, Double), (DenseVector[Double], DenseVector[Double])) = t2(1.0, DenseVector(2.0, 1.0))
    println(t2Res)

    def fx3(
        x1: DualNumberScalar[Double],
        x2: DualNumberColumnVector[DenseVector[Double]]
    ): (DualNumberScalar[Double], DualNumberColumnVector[DenseVector[Double]]) = (x1, x2)

    val t3 = ScalaGrad.derive(fx3.tupled)
    val t3Res: ((Double, DenseVector[Double]), (DenseVector[Double], DenseMatrix[Double])) = t3(1.0, DenseVector(2.0, 1.0))
    println(t3Res)

    val a = DeriverBreezeDoubleForwardPlan.algebraT
    
     def fx4(
         x1: a.Scalar,
         x2: a.ColumnVector
     ): (a.Scalar, a.Matrix) = (x1, x2 * x2.t)

    val t4 = ScalaGrad.derive(fx4.tupled)
    val t4Res: ((Double, DenseMatrix[Double]), (DenseVector[Double], DenseMatrix[Double])) = t4(1.0, DenseVector(2.0, 1.0))
    println("t4Res")
    println(t4Res)
    println(t4Res._1._2.rows + " " + t4Res._1._2.cols)
    println(t4Res._2._2.rows + " " + t4Res._2._2.cols)

     def fx5(
         x1: a.Scalar,
     ): (a.Scalar, a.Scalar) = (x1, x1)

    val t5 = ScalaGrad.derive(fx5)
    val t5Res: (Double, Double) = t5(1.0)
    println("t5Res")
    println(t5Res)

@main
def reverseMulti = 
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.given
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.algebra.*
    import scalagrad.api.reverse.dual.*

    val a = DeriverBreezeDoubleReversePlan.algebraT

    def fx(
        x1: a.Scalar,
        x2: a.Scalar,
        x3: a.Scalar
    ): (a.Scalar, a.Scalar) = (x1, x2)

    val t = ScalaGrad.derive(fx.tupled)
    val tRes: ((Double, Double), (Double, Double), (Double, Double)) = t(1.0, 2.0, 3.0)
    println(tRes)

    def fx2(
        x1: a.Scalar,
        x2: a.ColumnVector
    ): (a.Scalar, a.Scalar) = (x1, x1)

    val t2 = ScalaGrad.derive(fx2.tupled)
    val t2Res: ((Double, Double), (DenseVector[Double], DenseVector[Double])) = t2(1.0, DenseVector(2.0, 1.0))
    println(t2Res)

     def fx3(
         x1: a.Scalar,
         x2: a.ColumnVector
     ): (a.Scalar, a.ColumnVector) = (x1, x2)

    val t3 = ScalaGrad.derive(fx3.tupled)
    val t3Res: ((Double, DenseVector[Double]), (DenseVector[Double], DenseMatrix[Double])) = t3(1.0, DenseVector(2.0, 1.0))
    println(t3Res)

     def fx4(
         x1: a.Scalar,
         x2: a.ColumnVector
     ): (a.Scalar, a.Matrix) = (x1, x2 * x2.t)

    val t4 = ScalaGrad.derive(fx4.tupled)
    val t4Res: ((Double, DenseMatrix[Double]), (DenseVector[Double], DenseMatrix[Double])) = t4(1.0, DenseVector(2.0, 1.0))
    println("t4Res")
    println(t4Res)
    println(t4Res._1._2.rows + " " + t4Res._1._2.cols)
    println(t4Res._2._2.rows + " " + t4Res._2._2.cols)


    
@main
def forward = 
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan.given
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan.algebra.*
    import scalagrad.api.forward.dual.*

    def e(
        x1: DualNumberScalar[Double],
    ): DualNumberScalar[Double] = x1

    val de = ScalaGrad.derive(e)
    println(de(1.0))

    def f(
        x1: DualNumberScalar[Double],
        x2: DualNumberScalar[Double]
    ): DualNumberScalar[Double] = x2 * x1
    val df = ScalaGrad.derive(f)
    println(df(1.0, 2.0))

    def f2(
        x1: DualNumberMatrix[DenseMatrix[Double]],
        x2: DualNumberMatrix[DenseMatrix[Double]]
    ): DualNumberScalar[Double] = 
        (x2 * x1).sum

    val df2 = ScalaGrad.derive(f2)
    println(df2(
        DenseMatrix((1.0, 2.0), (3.0, 4.0)),
        DenseMatrix((1.0, 2.0), (3.0, 4.0))
    ))
    
    val dgWithTypeClass = ScalaGrad.derive(gWithTypeClass[
        DualNumberScalar[Double], DualNumberColumnVector[DenseVector[Double]], DualNumberRowVector[Transpose[DenseVector[Double]]], DualNumberMatrix[DenseMatrix[Double]],
    ])
    println(dgWithTypeClass(1.0, 2.0))
    
    val dgWithTypeDependency = ScalaGrad.derive(gWithTypeDependency(DeriverBreezeDoubleForwardPlan.algebraT))
    println(dgWithTypeDependency(1.0, 2.0))
    
    println("DONE")


@main
def reverse = 
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.*
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.given
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.algebra.*
    import scalagrad.api.reverse.dual.*

    val alg = DeriverBreezeDoubleReversePlan.algebraT

    def f(
        x1: alg.Scalar,
        x2: alg.Scalar
    ): alg.Scalar = x1 * x2

    val df = ScalaGrad.derive(f)
    println(df(1.0, 2.0))

    def f2(
        x1: alg.Matrix,
        x2: alg.Matrix
    ): alg.Scalar = 
        (x2 * x1).sum

    val df2 = ScalaGrad.derive(f2)
    println(df2(
        DenseMatrix((1.0, 2.0), (3.0, 4.0)),
        DenseMatrix((1.0, 2.0), (3.0, 4.0))
    ))

    def f3(
        x1: alg.Scalar,
        x2: alg.Scalar,
        x3: alg.Scalar,
        x4: alg.Scalar,
        x5: alg.Scalar,
        x6: alg.Scalar,
        x7: alg.Scalar
    ): alg.Scalar = x1 * x2 * x3 * x4
    
    val df3 = ScalaGrad.derive(f3)
    println(df3(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))

    println("DONE")


@main
def forwardForward =
    import spire.algebra.Trig
    import scalagrad.api.forward.DeriverForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import DeriverBreezeDoubleForwardPlan.{algebraGiven => _, given}
    import scalagrad.api.forward.dual.*
    import spire.implicits._
    import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
    import scalagrad.api.spire.trig.DualScalarIsTrig.given
    // construct DeriverPlan for deriving twice by chaining plan
    val ffp = new DeriverForwardPlan(DeriverBreezeDoubleForwardPlan.algebraGiven)
    import ffp.given

    def f(x: ffp.algebraT.Scalar): ffp.algebraT.Scalar = x * x
    // Apply ScalaGrad.derive twice on function
    val ddf = ScalaGrad.derive(ScalaGrad.derive(f))
    println(ddf(5.0))

    // Example with Trig
    def g(x: ffp.algebraT.Scalar)(
        using trig: Trig[ffp.algebraT.Scalar]
    ): ffp.algebraT.Scalar = trig.exp(x)
    val ddg = ScalaGrad.derive(ScalaGrad.derive(g))
    println(ddg(5.0))

    def f2(x1: ffp.algebraT.Scalar, x2: ffp.algebraT.Scalar): ffp.algebraT.Scalar = x1 * x2
    // Apply ScalaGrad.derive twice on function
    val df2: Tuple2[DualNumberScalar[Double], DualNumberScalar[Double]] => Tuple2[DualNumberScalar[Double], DualNumberScalar[Double]] = ScalaGrad.derive(f2)
    import DeriverBreezeDoubleForwardPlan.{algebraGiven => _, given}
    val ddf2 = ScalaGrad.derive(df2)
    println(ddf2(1.0, 1.0))
