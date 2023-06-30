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
    // TODO bring multiple derivation to work
    import scalagrad.api.forward.DeriverForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    // val ffp = new DeriverForwardPlan(DeriverBreezeDoubleForwardPlan.algebraGiven)
    // val alg = ffp.algebraT
    // println(alg.Scalar)
    // def f(
    //     x1: alg.Scalar,
    //     x2: alg.Scalar
    // ): alg.Scalar = x1 * x2
    // import ffp.*
    // ScalaGrad.derive(f)