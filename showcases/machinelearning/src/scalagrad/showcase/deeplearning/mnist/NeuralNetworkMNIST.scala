package scalagrad.showcase.deeplearning.mnist

import breeze.linalg.*
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.reverse.ReverseMode
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
import scalagrad.showcase.deeplearning.mnist.MNISTDataSet.MNISTEntry
import scalagrad.showcase.deeplearning.mnist.Util.*
import spire.math.Numeric
import spire.std.double.*
import spire.syntax.all.trigOps
import spire.syntax.numeric.partialOrderOps

import scala.annotation.targetName
import scala.io.Source

// Extend Reverse mode for missing case
extension (rad: ReverseMode.type)
    @targetName("deriveCVMCVM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) = 
        alg => (cv1, m1, cv2, m2) =>
            val mode = ReverseMode.dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(cv1, m1, cv2, m2).asInstanceOf[(alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix)]

object NeuralNetworkMNIST:

    def neuralNetwork(using alg: MatrixAlgebraDSL)(
        xs: alg.Matrix,
        firstW0: alg.ColumnVector, 
        firstWs: alg.Matrix,
        lastW0: alg.ColumnVector,
        lastWs: alg.Matrix,
    ): alg.Matrix = 
        val h = (xs * firstWs + firstW0.t).map(relu)
        (h * lastWs + lastW0.t)
            .mapRows(row => softmax(row.t).t)

    def relu[P: Numeric](x: P): P = 
        val num = summon[Numeric[P]]
        if x < num.zero then num.zero else x

    def softmax(using alg: MatrixAlgebraDSL)(x: alg.ColumnVector): alg.ColumnVector = 
        def unstableSoftmax(x: alg.ColumnVector): alg.ColumnVector = 
            val exps = x.map(_.exp)
            exps / exps.sum
        val maxElement = x.elements.maxBy(_.toDouble)
        unstableSoftmax(x - maxElement)

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
            val dLoss = ReverseMode.derive(loss(xsBatch, ysBatch))(BreezeDoubleMatrixAlgebraDSL)
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
            
            miniBatchGradientDescent(data.tail)(
                firstW0 - lr * dFirstW0,
                firstWs - lr * dFirstWs,
                lastW0 - lr * dLastW0,
                lastWs - lr * dLastWs, 
                lr,
                n - 1,
            )
    
    def loss(xs: DenseMatrix[Double], ys: DenseMatrix[Double])(alg: MatrixAlgebraDSL)(
        firstW0: alg.ColumnVector,
        firstWs: alg.Matrix,
        lastW0: alg.ColumnVector,
        lastWs: alg.Matrix,
    ): alg.Scalar =
        val ysHat = neuralNetwork(using alg)(alg.lift(xs), firstW0, firstWs, lastW0, lastWs)
        crossEntropy(using alg)(alg.lift(ys), ysHat)

    def crossEntropy(using alg: MatrixAlgebraDSL)(ys: alg.Matrix, ysHat: alg.Matrix): alg.Scalar =
        def clip(x: alg.Scalar): alg.Scalar =
            val epsilon: Double = 1e-07
            val minS = alg.lift(epsilon)
            val maxS = alg.lift(1.0 - epsilon)
            if x < minS then minS
            else if x > maxS then maxS
            else x
        val logYsHat = ysHat.map(clip).map(_.log)
        val logYsHatYs = logYsHat *:* ys
        -(logYsHatYs.sum / alg.lift(logYsHat.nRows))

    def accuracy(yHatProp: DenseMatrix[Double], yM: DenseMatrix[Double]): Double =
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
        val ysHatTest = xsTest.map(xs => neuralNetwork(using BreezeDoubleMatrixAlgebraDSL)(xs, firstW0, firstWs, lastW0, lastWs))
        val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
        val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
        val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(using BreezeDoubleMatrixAlgebraDSL)(ys, ysHat)).toList
        val lossTest = lossTestBatch.sum / lossTestBatch.length

        println(
            List(
                f"testLoss=${lossTest}%.1f",
                f"testAcc=${accuracyTest * 100}%3f",
            ).mkString("\t")
        )
