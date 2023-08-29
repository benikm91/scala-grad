package scalagrad.showcase.linearregression.newtonmethod

import scalagrad.showcase.linearregression.*

import breeze.linalg.{DenseMatrix, DenseVector}
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

import scala.annotation.tailrec
import scala.io.Source

/**
 * The newton method applied to optimization problems shows an example for higher-order derivatives.
 * 
 * Generally the newton method is just a root finding algorithm, so finding x where f(x) = 0 for a given function f.
 * The newton method can be applied to optimization problems, by finding the roots of df, the first-derivative of f. 
 * The roots of df are the minima, sattel points and maxima of f.
 * 
 * For a convex optimization problem (like linear regression), the first derivative is only zero at the global minimum, therefore finidng the root of df is equivalent to finding the global minimum of f.
 * The newton method needs the derivative of the function to find its roots, therefore for an optimization problem, we need to derive the first derivative of the loss function, which is the second derivative.
 */
@main def newtonMethod() = 

    // Load and scale data

    val fishs = FishDataSet.load

    var (xsScala, _, _) = 
        StandardScaler.scaleMatrix(
            fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
        )
    val nFeatures = xsScala(0).length
    val xs = new DenseMatrix(xsScala.head.length, xsScala.length, xsScala.flatten.toArray).t

    val ysUnscaled = fishs.map(_.weight).toVector
    val (ysScala, ysMean, ysStd) = StandardScaler.scaleColumn(ysUnscaled)
    val ys = DenseVector(ysScala.toArray)
    
    // Define model, MSE metric and loss function.

    def linearModel(using alg: MatrixAlgebraDSL)(xs: alg.Matrix, w0: alg.Scalar, ws: alg.ColumnVector): alg.ColumnVector = 
        (xs * ws) + w0

    def meanSquaredError(using alg: MatrixAlgebraDSL)(ys: alg.ColumnVector, ysHat: alg.ColumnVector): alg.Scalar =
        (ys - ysHat).map(x => x * x).sum / alg.lift(ys.length * 2)

    def loss(xs: DenseMatrix[Double], ys: DenseVector[Double])(alg: MatrixAlgebraDSL)(w0: alg.Scalar, ws: alg.ColumnVector): alg.Scalar =
        val ysHat = linearModel(using alg)(alg.lift(xs), w0, ws)
        meanSquaredError(using alg)(alg.lift(ys), ysHat)

    // Implement newton method

    def newtonMethod(w0: Double, ws: DenseVector[Double])(
        dLoss: (Double, DenseVector[Double]) => (Double, DenseVector[Double]),
        d2Loss: (Double, DenseVector[Double]) => ((Double, DenseVector[Double]), (DenseVector[Double], DenseMatrix[Double]))
    ): (Double, DenseVector[Double]) =
        import breeze.linalg.*
        val (dW0, dWs) = dLoss(w0, ws)
        val ((ddW0, ddW0Ws), (ddWsW0, ddWs)) = d2Loss(w0, ws)
        val dWsT = dWs.t
        val ddW0Inv = 1.0 / ddW0
        val ddWsInv = inv(ddWs)
        val w0New = w0 - (ddW0Inv * dW0)
        val wsNew = ws - (ddWsInv * dWs)
        (w0New, wsNew)

    def rootMeanSquaredError(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        Math.sqrt(meanSquaredError(using BreezeDoubleMatrixAlgebraDSL)(ys, ysHat))

    val (initW0, initWs) = (0.0, DenseVector.fill(nFeatures)(0.0))

    val initYsHat = StandardScaler.inverseScaleColumn(
        linearModel(using BreezeDoubleMatrixAlgebraDSL)(xs, initW0, initWs).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(initYsHat.toArray))}g  -- RMSE with initial weights")

    // Derive function twice and call newton method 

    println("Forward-Forward mode")
    import scalagrad.api.forward.ForwardMode.derive as d
    val dLoss = d(loss(xs, ys))
    val ddLoss = d(dLoss)
    // For linear regression we only have to apply the newton method once to find the minimum (because the second derivative is constant).
    val (w0, ws) = newtonMethod(initW0, initWs)(dLoss(BreezeDoubleMatrixAlgebraDSL), ddLoss(BreezeDoubleMatrixAlgebraDSL))

    val ysHat = StandardScaler.inverseScaleColumn(
        linearModel(using BreezeDoubleMatrixAlgebraDSL)(xs, w0, ws).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(ysHat.toArray))}g  -- RMSE with learned weights")