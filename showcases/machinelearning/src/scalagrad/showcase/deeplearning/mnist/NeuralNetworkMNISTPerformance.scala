package scalagrad.showcase.deeplearning.mnist

import breeze.linalg.*
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.reverse.ReverseMode
import scalagrad.auto.breeze.BreezeFloatMatrixAlgebraDSL
import scalagrad.showcase.deeplearning.mnist.MNISTDataSet.MNISTEntry
import scalagrad.showcase.deeplearning.mnist.Util.*
import spire.math.Numeric
import spire.std.double.*
import spire.syntax.all.trigOps
import spire.syntax.numeric.partialOrderOps

import scala.annotation.targetName
import scala.io.Source

object NeuralNetworkMNISTPerformance:

    /**
     * MNIST showcase with performance measurements to compare to other automatic differentiation libraries.
     * Notes for correct comparision:
     * - We are using float point precision
     * - MNIST data is preloded into memory
     */

    import TimeUtil.*

    @main def neuralNetworkReverseModePerformance() =

        import NeuralNetworkMNIST.*

        // CONFIG
        val batchSize = 64
        val nHiddenUnits = 36
        val epochs = 1

        val nFeatures = MNISTDataSet.nFeatures
        val nOutputUnits = MNISTDataSet.nLabels

        val (xsTrainDouble, ysTrainDouble) = preprocess(MNISTDataSet.loadTrain, batchSize)
        val xsTrain = xsTrainDouble.map(_.map(_.toFloat))
        val ysTrain = ysTrainDouble.map(_.map(_.toFloat))
        val eagerData = cycle(xsTrain.zip(ysTrain).toList)
                
        def getRandomWeights(nFeatures: Int, nHiddenUnits: Int, nOutputUnits: Int): (
            DenseVector[Float],
            DenseMatrix[Float],
            DenseVector[Float],
            DenseMatrix[Float],
        ) = 
            val rand = scala.util.Random(42)
            (
                DenseVector.fill(nHiddenUnits)(rand.nextFloat() - 0.5f),
                DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextFloat() - 0.5f),
                DenseVector.fill(nOutputUnits)(rand.nextFloat() - 0.5f),
                DenseMatrix.fill(nHiddenUnits, nOutputUnits)(rand.nextFloat() - 0.5f),
            )
        
        def loss(xs: DenseMatrix[Float], ys: DenseMatrix[Float])(alg: MatrixAlgebraDSL)(
            firstW0: alg.ColumnVector,
            firstWs: alg.Matrix,
            lastW0: alg.ColumnVector,
            lastWs: alg.Matrix,
        ): alg.Scalar =
            val ysHat = neuralNetwork(using alg)(alg.lift(xs), firstW0, firstWs, lastW0, lastWs)
            crossEntropy(using alg)(alg.lift(ys), ysHat)

        def miniBatchGradientDescent
        (data: LazyList[(DenseMatrix[Float], DenseMatrix[Float])])
        (
            firstW0: DenseVector[Float],
            firstWs: DenseMatrix[Float],
            lastW0: DenseVector[Float],
            lastWs: DenseMatrix[Float],
            lr: Float,
            n: Int,
        ): (DenseVector[Float], DenseMatrix[Float], DenseVector[Float], DenseMatrix[Float]) =
            if n == 0 then (firstW0, firstWs, lastW0, lastWs)
            else
                val (xsBatch, ysBatch) = data.head
                val dLoss = ReverseMode.derive(loss(xsBatch, ysBatch))(BreezeFloatMatrixAlgebraDSL)
                val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
                
                miniBatchGradientDescent(data.tail)(
                    firstW0 - lr * dFirstW0,
                    firstWs - lr * dFirstWs,
                    lastW0 - lr * dLastW0,
                    lastWs - lr * dLastWs, 
                    lr,
                    n - 1,
                )
    
        val ((firstW0, firstWs, lastW0, lastWs), measurements) = (1 to epochs).foldLeft((getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits), List.empty[Double])) { case (((firstW0, firstWs, lastW0, lastWs), measurements), epoch) =>
            val iters = Math.max(MNISTDataSet.trainSize / batchSize, 1)
            println("running epoch " + epoch)
            val (res, ds) = timeMeasure {
                miniBatchGradientDescent(eagerData)(firstW0, firstWs, lastW0, lastWs, 0.01, iters)
            }
            (res, ds :: measurements)
        }

        println(measurements.mkString(","))

        def accuracy(yHatProp: DenseMatrix[Float], yM: DenseMatrix[Float]): Float =
            val yHat = yHatProp(*, ::).map(x => argmax(x))
            val y = yM(*, ::).map(x => argmax(x))
            val correct = yHat.toArray.zip(y.toArray).map((yHat, y) => if yHat == y then 1 else 0).sum
            correct.toFloat / yHat.length

        val (xsTestDouble, ysTestDouble) = preprocess(MNISTDataSet.loadTest, 32)
        val xsTest = xsTestDouble.map(_.map(_.toFloat))
        val ysTest = ysTestDouble.map(_.map(_.toFloat))
        val ysHatTest = xsTest.map(xs => neuralNetwork(using BreezeFloatMatrixAlgebraDSL)(xs, firstW0, firstWs, lastW0, lastWs))
        val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
        val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
        val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(using BreezeFloatMatrixAlgebraDSL)(ys, ysHat)).toList
        val lossTest = lossTestBatch.sum / lossTestBatch.length

        println(
            List(
                f"testLoss=${lossTest}%.1f",
                f"testAcc=${accuracyTest * 100}%3f",
            ).mkString("\t")
        )


object TimeUtil:

    import java.util.concurrent.TimeUnit

    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        val ds = TimeUnit.SECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS)
        println("Elapsed time: " + (ds) + "s")
        result
    }

    def timeMeasure[R](block: => R, unit: TimeUnit = TimeUnit.SECONDS): (R, Double) = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        val ds = unit.convert(t1 - t0, TimeUnit.NANOSECONDS)
        (result, ds)
    }