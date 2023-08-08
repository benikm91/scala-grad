package scalagrad.showcase.deeplearning.mnist

import scalagrad.showcase.deeplearning.Util.{time, timeMeasure}
import scala.io.Source
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import scalagrad.api.ScalaGrad
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraT
import breeze.linalg.{DenseMatrix, DenseVector}
import spire.math.Numeric
import spire.algebra.Trig
import spire.implicits.*
import scalagrad.api.DeriverFromTo

import MNISTDataSet.MNISTEntry
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import scalagrad.api.forward.dual.DualNumberMatrix
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given
import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
import DeriverBreezeDoubleForwardPlan.given
import scalagrad.api.dual.DualMatrixAlgebraT
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.reverse.dual.DualDeltaScalar
import Util.*

import scalagrad.api.DualBreezeMatrixAlgebraT.*
import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan
import DeriverBreezeDoubleReversePlan.given

object NeuralNetworkMNIST:

    def neuralNetwork(ops: MatrixAlgebraT)(
        xs: ops.Matrix,
        firstW0: ops.ColumnVector, 
        firstWs: ops.Matrix,
        lastW0: ops.ColumnVector,
        lastWs: ops.Matrix,
    )(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
        val h = (xs * firstWs + firstW0.t).map(relu)
        (h * lastWs + lastW0.t)
            .mapRows(row => softmax(ops)(row.t).t)

    def relu[P: Numeric](x: P): P = 
        val num = summon[Numeric[P]]
        if x < num.zero then num.zero else x

    def softmax(ops: MatrixAlgebraT)(x: ops.ColumnVector)(using trig: Trig[ops.Scalar]): ops.ColumnVector = 
        def unstableSoftmax(ops: MatrixAlgebraT)(x: ops.ColumnVector)(using trig: Trig[ops.Scalar]): ops.ColumnVector = 
            val exps = x.map(trig.exp)
            exps / exps.sum
        val maxElement = x.elements.maxBy(_.toDouble)
        unstableSoftmax(ops)(x - maxElement)

    def miniBatchGradientDescent
    (data: LazyList[(DenseMatrix[Double], DenseMatrix[Double])])
    (
        firstW0: DenseVector[Double],
        firstWs: DenseMatrix[Double],
        lastW0: DenseVector[Double],
        lastWs: DenseMatrix[Double],
        lr: Double,
        n: Int,
    ): (DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            val (xsBatch, ysBatch) = data.head
            val algebra = DeriverBreezeDoubleReversePlan.algebraT
            val dLoss = ScalaGrad.derive(loss(algebra)(
                algebra.lift(xsBatch),
                algebra.lift(ysBatch)
            ))
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
            
            miniBatchGradientDescent(data.tail)(
                firstW0 - lr * dFirstW0,
                firstWs - lr * dFirstWs,
                lastW0 - lr * dLastW0,
                lastWs - lr * dLastWs, 
                lr,
                n - 1,
            )

    def loss(ops: MatrixAlgebraT)(xs: ops.Matrix, ys: ops.Matrix)(
        firstW0: ops.ColumnVector,
        firstWs: ops.Matrix,
        lastW0: ops.ColumnVector,
        lastWs: ops.Matrix,
    )(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
        val ysHat = neuralNetwork(ops)(xs, firstW0, firstWs, lastW0, lastWs)
        crossEntropy(ops)(ys, ysHat)

    def crossEntropy(ops: MatrixAlgebraT)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Scalar =
        def clip(ops: MatrixAlgebraT)(x: ops.Scalar)(using n: Numeric[ops.Scalar]): ops.Scalar =
            val epsilon: Double = 1e-07
            val minS = ops.liftToScalar(epsilon)
            val maxS = ops.liftToScalar(1.0 - epsilon)
            if x < minS then minS
            else if x > maxS then maxS
            else x
        val logYsHat = ysHat.map(clip(ops)).map(trig.log)
        val logYsHatYs = logYsHat *:* ys
        ops.liftToScalar(-1) * ops.divideSS(logYsHatYs.sum, ops.liftToScalar(logYsHat.nRows))

    def accuracy(yHatProp: DenseMatrix[Double], yM: DenseMatrix[Double]): Double =
        import breeze.linalg.*
        val yHat = yHatProp(*, ::).map(x => argmax(x))
        val y = yM(*, ::).map(x => argmax(x))
        val correct = yHat.toArray.zip(y.toArray).map((yHat, y) => if yHat == y then 1 else 0).sum
        correct.toDouble / yHat.length

    def getRandomWeights(nFeatures: Int, nHiddenUnits: Int, nOutputUnits: Int): (
        DenseVector[Double],
        DenseMatrix[Double],
        DenseVector[Double],
        DenseMatrix[Double],
    ) = 
        val rand = scala.util.Random(42)
        (
            DenseVector.fill(nHiddenUnits)(rand.nextDouble() - 0.5),
            DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble() - 0.5),
            DenseVector.fill(nOutputUnits)(rand.nextDouble() - 0.5),
            DenseMatrix.fill(nHiddenUnits, nOutputUnits)(rand.nextDouble() - 0.5),
        )
    
    @main def neuralNetworkReverseMode() =

        // CONFIG
        val batchSize = 64
        val nHiddenUnits = 36
        val epochs = 1

        val nFeatures = MNISTDataSet.nFeatures
        val nOutputUnits = MNISTDataSet.nLabels
        val iters = Math.max(MNISTDataSet.trainSize / batchSize, 1) * epochs

        val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
        val eagerData = cycle(xsTrain.zip(ysTrain).toList)
        
        val (initFirstW0, initFirstWs, initLastW0, initLastWs) = getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits)
        
        val (firstW0, firstWs, lastW0, lastWs) = miniBatchGradientDescent(eagerData)
            (initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters)

        val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, 32)
        val ysHatTest = xsTest.map(xs => neuralNetwork(BreezeDoubleMatrixAlgebraT)(xs, firstW0, firstWs, lastW0, lastWs))
        val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
        val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
        val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(BreezeDoubleMatrixAlgebraT)(ys, ysHat)).toList
        val lossTest = lossTestBatch.sum / lossTestBatch.length

        println(
            List(
                f"testLoss=${lossTest}%.1f",
                f"testAcc=${accuracyTest * 100}%3f",
            ).mkString("\t")
        )