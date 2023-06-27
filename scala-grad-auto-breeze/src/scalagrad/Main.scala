package scalagrad

import scalagrad.api.ScalaGrad
import scalagrad.api.DeriverFromTo
import breeze.linalg.DenseMatrix
import scala.util.TupledFunction


@main
def forward = 
    import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan.given
    import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan.algebra.*
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
    
    println("DONE")


@main
def reverse = 
    import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan
    import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan.*
    import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan.given
    import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan.algebra.*
    import scalagrad.api.reverse.dual.*

    def f(
        x1: BreezeDualScalar,
        x2: BreezeDualScalar
    ): BreezeDualScalar = x1 * x2

    val df = ScalaGrad.derive(f)
    println(df(1.0, 2.0))

    def f2(
        x1: BreezeDualMatrix,
        x2: BreezeDualMatrix
    ): BreezeDualScalar = 
        (x2 * x1).sum

    val df2 = ScalaGrad.derive(f2)
    println(df2(
        DenseMatrix((1.0, 2.0), (3.0, 4.0)),
        DenseMatrix((1.0, 2.0), (3.0, 4.0))
    ))

    def f3(
        x1: BreezeDualScalar,
        x2: BreezeDualScalar,
        x3: BreezeDualScalar,
        x4: BreezeDualScalar,
        x5: BreezeDualScalar,
        x6: BreezeDualScalar,
        x7: BreezeDualScalar
    ): BreezeDualScalar = x1 * x2 * x3 * x4
    
    val df3 = ScalaGrad.derive(f3)
    println(df3(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))

    println("DONE")

