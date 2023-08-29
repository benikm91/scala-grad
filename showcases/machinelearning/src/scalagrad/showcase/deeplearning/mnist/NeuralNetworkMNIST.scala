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
import scala.annotation.tailrec

/**
 * The Neural Network MNIST example shows that ScalaGrad can be used with fast performance for deep learning applications.
 * 
 * The example trains a 1-hidden layer neural network on the MNIST dataset containing small images.
 * First the neural network forward pass is implemented with the cross entropy loss. 
 * Then the mini batch gradient descent is implemented using ScalaGrad to derive the loss function.
 * 
 * Some use case specific details:
 * - We show how constants can be implemented by having a parameter group and using Scalas currying.
 * - We show how a wrapper function d can be made to support custom types (here Parameters).
 */

object NeuralNetworkMNIST:

    /**
     * Parameters of our neural network
     */
    case class Parameters[V, M](
        val firstW0: V, // hidden layer bias
        val firstWs: M, // hidden layer weights
        val lastW0: V,  // output layer bias
        val lastWs: M   // output layer weights
    )

    // Add helper derivative wrapper function for deriving function of type Parameters => Scalar
    def d(
        f: (alg: MatrixAlgebraDSL) => Parameters[alg.ColumnVector, alg.Matrix]=> alg.Scalar
    ): (alg: MatrixAlgebraDSL) => Parameters[alg.ColumnVector, alg.Matrix]=> Parameters[alg.ColumnVector, alg.Matrix]= 
        alg => p =>
            def fWrapper(alg: MatrixAlgebraDSL)(firstW0: alg.ColumnVector, firstWs: alg.Matrix, lastW0: alg.ColumnVector, lastWs: alg.Matrix) =
                f(alg)(Parameters(firstW0, firstWs, lastW0, lastWs))
            val mode = ReverseMode.dualMode(alg)
            val df = mode.derive(fWrapper(mode.algebraDSL).tupled)
            val (firstW0, firstWs, lastW0, lastWs) = df(p.firstW0, p.firstWs, p.lastW0, p.lastWs)
            Parameters(firstW0, firstWs, lastW0, lastWs)

    def neuralNetwork(using alg: MatrixAlgebraDSL)(xs: alg.Matrix, p: Parameters[alg.ColumnVector, alg.Matrix]): alg.Matrix =
        def relu[P](x: P)(using num: Numeric[P]): P = if x < num.zero then num.zero else x

        def softmax(using alg: MatrixAlgebraDSL)(x: alg.ColumnVector): alg.ColumnVector = 
            def unstableSoftmax(x: alg.ColumnVector): alg.ColumnVector = 
                val exps = x.map(_.exp)
                exps / exps.sum
            val maxElement = x.elements.maxBy(_.toDouble)
            unstableSoftmax(x - maxElement)
        val h = (xs * p.firstWs + p.firstW0.t).map(relu)
        (h * p.lastWs + p.lastW0.t)
            .mapRows(row => softmax(row.t).t)

    @tailrec
    def miniBatchGradientDescent
    (data: List[(DenseMatrix[Double], DenseMatrix[Double])])
    (
        p: Parameters[DenseVector[Double], DenseMatrix[Double]],
        lr: Double, // learning rate
    ): Parameters[DenseVector[Double], DenseMatrix[Double]] =
        if data.isEmpty then p
        else
            // get next batch
            val (xsBatch, ysBatch) = data.head

            // derive the loss function for the current batch
            val dLoss = d(loss(xsBatch, ysBatch))(BreezeDoubleMatrixAlgebraDSL)
            val dP = dLoss(p)
            
            miniBatchGradientDescent(data.tail)(
                // apply gradient descent update on weights
                Parameters(
                    p.firstW0 - lr * dP.firstW0,
                    p.firstWs - lr * dP.firstWs,
                    p.lastW0 - lr * dP.lastW0,
                    p.lastWs - lr * dP.lastWs, 
                ),
                lr,
            )
    
    // loss takes first the data in the first parameter group, which are constants from automatic differentiation perspective.
    // loss(xs, ys), where the data is fixed, can then be derived by function d (defined above).
    def loss(xs: DenseMatrix[Double], ys: DenseMatrix[Double])(alg: MatrixAlgebraDSL)(
        p: Parameters[alg.ColumnVector, alg.Matrix]
    ): alg.Scalar =
        given alg.type = alg
        val ysHat = neuralNetwork(using alg)(alg.lift(xs), p)
        crossEntropy(using alg)(alg.lift(ys), ysHat)

    // Cross entropy metric 
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

    def getRandomParameters(nFeatures: Int, nHiddenUnits: Int, nOutputUnits: Int): 
        Parameters[DenseVector[Double], DenseMatrix[Double]] = 
        val rand = scala.util.Random(42)
        Parameters(
            DenseVector.fill(nHiddenUnits)(rand.nextDouble() - 0.5),
            DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble() - 0.5),
            DenseVector.fill(nOutputUnits)(rand.nextDouble() - 0.5),
            DenseMatrix.fill(nHiddenUnits, nOutputUnits)(rand.nextDouble() - 0.5),
        )

    // Print current performance of Parameters on test data
    def logPerformance(xsTest: Seq[DenseMatrix[Double]], ysTest: Seq[DenseMatrix[Double]])(p: Parameters[DenseVector[Double], DenseMatrix[Double]]): Unit =
        given BreezeDoubleMatrixAlgebraDSL.type = BreezeDoubleMatrixAlgebraDSL
        val ysHatTest = xsTest.map(xs => neuralNetwork(xs, p))
        val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
        val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
        val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(ys, ysHat)).toList
        val lossTest = lossTestBatch.sum / lossTestBatch.length
        println(
            List(
                f"testLoss=${lossTest}%.1f",
                f"testAcc=${accuracyTest * 100}%3f",
            ).mkString("\t")
        )

    @main def neuralNetworkReverseMode() =

        // CONFIG
        val batchSize = 64
        val nHiddenUnits = 36
        val epochs = 2

        val nFeatures = MNISTDataSet.nFeatures
        val nOutputUnits = MNISTDataSet.nLabels
        val epochIters = Math.max(MNISTDataSet.trainSize / batchSize, 1)

        // Eagerly load MNIST data into memory
        val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
        val eagerData = xsTrain.zip(ysTrain).toList
     
        val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, 32)

        // Initialize weights
        val initialParams = getRandomParameters(nFeatures, nHiddenUnits, nOutputUnits)
        
        // Run miniBatchGradientDescent {epochs} number of times
        val finalParams = 
            (1 to epochs).foldLeft(initialParams) {
                case (currentParams, epoch) => {
                    println(f"epoch ${epoch}")
                    val nextParams = miniBatchGradientDescent(eagerData)(currentParams, 0.01)
                    logPerformance(xsTest, ysTest)(nextParams)
                    (nextParams)
                }
            }
