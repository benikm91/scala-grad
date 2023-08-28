package scalagrad.showcase.linearregression.newtonmethod

import scalagrad.showcase.linearregression.*

import breeze.linalg.{DenseMatrix, DenseVector}
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

import scala.annotation.tailrec
import scala.io.Source

@main def newtonMethod() = 

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
    
    def linearModel(using alg: MatrixAlgebraDSL)(xs: alg.Matrix, w0: alg.Scalar, ws: alg.ColumnVector): alg.ColumnVector = 
        (xs * ws) + w0

    def meanSquaredError(using alg: MatrixAlgebraDSL)(ys: alg.ColumnVector, ysHat: alg.ColumnVector): alg.Scalar =
        (ys - ysHat).map(x => x * x).sum / alg.lift(ys.length * 2)

    def loss(xs: DenseMatrix[Double], ys: DenseVector[Double])(alg: MatrixAlgebraDSL)(w0: alg.Scalar, ws: alg.ColumnVector): alg.Scalar =
        val ysHat = linearModel(using alg)(alg.lift(xs), w0, ws)
        meanSquaredError(using alg)(alg.lift(ys), ysHat)

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

    println("Forward-Forward mode")
    import scalagrad.api.forward.ForwardMode.derive as d
    val dLoss = d(loss(xs, ys))
    val ddLoss = d(dLoss)
    val (w0, ws) = newtonMethod(initW0, initWs)(dLoss(BreezeDoubleMatrixAlgebraDSL), ddLoss(BreezeDoubleMatrixAlgebraDSL))

    val ysHat = StandardScaler.inverseScaleColumn(
        linearModel(using BreezeDoubleMatrixAlgebraDSL)(xs, w0, ws).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(ysHat.toArray))}g  -- RMSE with learned weights")