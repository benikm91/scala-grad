package scalagrad.showcase.linearregression

import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import breeze.linalg.{DenseMatrix, DenseVector}
import scalagrad.api.ScalaGrad
import scalagrad.api.dual.DualMatrixAlgebraT
import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraT
import scala.annotation.tailrec

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
    
    def linearModel(alg: MatrixAlgebraT)(xs: alg.Matrix, w0: alg.Scalar, ws: alg.ColumnVector): alg.ColumnVector = 
        (xs * ws) + w0

    def meanSquaredError(alg: MatrixAlgebraT)(ys: alg.ColumnVector, ysHat: alg.ColumnVector): alg.Scalar =
        (ys - ysHat).map(x => x * x).sum / alg.liftToScalar(ys.length * 2)

    def loss(alg: MatrixAlgebraT)(xs: alg.Matrix, ys: alg.ColumnVector)(w0: alg.Scalar, ws: alg.ColumnVector): alg.Scalar =
        val ysHat = linearModel(alg)(xs, w0, ws)
        meanSquaredError(alg)(ys, ysHat)

    def newtonMethod(alg: MatrixAlgebraT)(w0: Double, ws: DenseVector[Double])(
        dLoss: ((Double, DenseVector[Double])) => (Double, DenseVector[Double]),
        d2Loss: ((Double, DenseVector[Double])) => ((Double, DenseVector[Double]), (DenseVector[Double], DenseMatrix[Double]))
    ): (Double, DenseVector[Double]) =
        import breeze.linalg._
        val (dW0, dWs) = dLoss(w0, ws)
        val ((ddW0, ddW0Ws), (ddWsW0, ddWs)) = d2Loss(w0, ws)
        val dWsT = dWs.t
        val ddW0Inv = 1.0 / ddW0
        val ddWsInv = inv(ddWs)
        val w0New = w0 - (ddW0Inv * dW0)
        val wsNew = ws - (ddWsInv * dWs)
        (w0New, wsNew)

    def rootMeanSquaredError(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        Math.sqrt(meanSquaredError(BreezeDoubleMatrixAlgebraT)(ys, ysHat))

    val (initW0, initWs) = (0.0, DenseVector.fill(nFeatures)(0.0))

    val initYsHat = StandardScaler.inverseScaleColumn(
        linearModel(BreezeDoubleMatrixAlgebraT)(xs, initW0, initWs).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(initYsHat.toArray))}g  -- RMSE with initial weights")

    import scalagrad.api.forward.ForwardMode
    import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
    import BreezeDoubleForwardMode.{algebraT => alg}
    import BreezeDoubleForwardMode.given

    println("Forward-Forward mode")
    val dLoss = ScalaGrad.derive(loss(alg)(
        alg.lift(xs), 
        alg.lift(ys)
    ))
    val ffad = new ForwardMode(BreezeDoubleForwardMode.algebra)
    val alg2 = ffad.algebraT
    import ffad.given

    import BreezeDoubleForwardMode.given // TODO why is this needed?
    val d2Loss = ScalaGrad.derive(
        ScalaGrad.derive(loss(alg2)(
            alg2.lift(alg.lift(xs)), 
            alg2.lift(alg.lift(ys))
        ))
    )
    val (w0, ws) = newtonMethod(BreezeDoubleMatrixAlgebraT)(initW0, initWs)(dLoss, d2Loss)

    val ysHat = StandardScaler.inverseScaleColumn(
        linearModel(BreezeDoubleMatrixAlgebraT)(xs, w0, ws).toScalaVector, 
        ysMean, ysStd
    )
    println(f"${rootMeanSquaredError(DenseVector(ysUnscaled.toArray), DenseVector(ysHat.toArray))}g  -- RMSE with learned weights")