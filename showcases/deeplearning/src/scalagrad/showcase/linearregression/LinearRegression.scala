package scalagrad.showcase.linearregression

/*
import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.ScalaGrad
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
import scala.annotation.tailrec

import breeze.linalg.{Vector => _, *}

@main def linearRegression() = 

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
        (ys - ysHat).map(x => x * x).sum / alg.liftToScalar(ys.length * 2)

    def loss(using alg: MatrixAlgebraDSL)(xs: alg.Matrix, ys: alg.ColumnVector)(w0: alg.Scalar, ws: alg.ColumnVector): alg.Scalar =
        val ysHat = linearModel(xs, w0, ws)
        meanSquaredError(ys, ysHat)

    def gradientDescent(
        xs: DenseMatrix[Double],
        ys: DenseVector[Double],
        w0: Double,
        ws: DenseVector[Double],
        lr: Double, // learn rate
        n: Int,
    ): (Double, DenseVector[Double]) =
        // create dLoss with auto differentiation (reverse mode)
        val alg = BreezeDoubleReverseMode.algebraT
        val dLoss = ScalaGrad.derive(loss(using alg)(
                alg.lift(xs),
                alg.lift(ys)
            ))
        // implement gradient descent recursively using dLoss
        @tailrec
        def iterate(
            w0: Double,
            ws: DenseVector[Double],
            n: Int
        ): (Double, DenseVector[Double]) =
            if n == 0 then (w0, ws)
            else
                val (dW0, dWs) = dLoss(w0, ws)
                iterate(
                    w0 - lr * dW0,
                    ws - lr * dWs,
                    n - 1
                )
        iterate(w0, ws, n)

    def rootMeanSquaredError(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        Math.sqrt(meanSquaredError(using BreezeDoubleMatrixAlgebraDSL)(ys, ysHat))

    val (initW0, initWs) = (0.0, DenseVector.fill(nFeatures)(0.0))

    val initYsHat = StandardScaler.inverseScaleColumn(
        linearModel(using BreezeDoubleMatrixAlgebraDSL)(xs, initW0, initWs).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(initYsHat.toArray))}g  -- RMSE with initial weights")

    val (w0, ws) = gradientDescent(xs, ys, initW0, initWs, 0.01, 100)

    val ysHat = StandardScaler.inverseScaleColumn(
        linearModel(using BreezeDoubleMatrixAlgebraDSL)(xs, w0, ws).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(ysHat.toArray))}g  -- RMSE with learned weights")*/