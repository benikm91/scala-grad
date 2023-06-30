package scalagrad.showcase.deeplearning.mnist

import scalagrad.showcase.deeplearning.Util.{time, timeMeasure}
import scala.io.Source
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import scalagrad.api.ScalaGrad
import scalagrad.auto.breeze.BreezeMatrixAlgebraT
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
import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan
import DeriverBreezeForwardPlan.given
import scalagrad.api.dual.DualMatrixAlgebraT
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.reverse.dual.DualDeltaScalar

def normalize(pixel: Double) = pixel / 255

def preprocess(data: Iterator[MNISTEntry], batchSize: Int) = 
    val dataL = LazyList.from(data)
    val xs = dataL.map(entry => entry.pixels.map(normalize).toVector)
    val ys = dataL.map(_.label)

    val xsBatches = LazyList.from(
        xs.grouped(batchSize)
            .map(xBatch => new DenseMatrix(xBatch.head.length, xBatch.length, xBatch.flatten.toArray).t)
    )
    val ysBatches = LazyList.from(
        ys.grouped(batchSize)
            .map(yBatch => DenseVector(yBatch.toArray))
            .map(yBatch => {
                DenseMatrix.tabulate(yBatch.length, MNISTDataSet.nLabels) { (i, j) =>
                    if yBatch(i) == j then 1.0 else 0.0
                }
            })
    )
    (xsBatches, ysBatches)

def relu[P: Numeric](x: P): P = 
    val num = summon[Numeric[P]]
    if num.lt(x, num.zero) then num.zero else x

def softmax(ops: MatrixAlgebraT)(x: ops.ColumnVector)(using trig: Trig[ops.Scalar]): ops.ColumnVector = 
    import ops.*
    val exps = x.map(x => trig.exp(x))
    val sumExps = exps.sum
    ops.divideCVS(exps, sumExps)

def stableSoftmax(ops: MatrixAlgebraT)(x: ops.ColumnVector)(using trig: Trig[ops.Scalar]): ops.ColumnVector = 
    val maxElement = x.elements.maxBy(_.toDouble)
    softmax(ops)(x - maxElement)

def neuralNetworkApply(ops: MatrixAlgebraT)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.t
    val hh = h.map(relu)
    val o = hh * lastWs + lastW0.t
    val res = o.mapRows(row => stableSoftmax(ops)(row.t).t)
    res

def neuralNetwork(ops: DualMatrixAlgebraT)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using 
    num: Numeric[ops.Scalar], 
    num2: Numeric[ops.PrimaryScalar], 
    trig: Trig[ops.Scalar],
    deriver: DeriverFromTo[ops.Scalar => ops.Scalar, ops.PrimaryScalar => ops.PrimaryScalar],
): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.t
    val dRelu = ScalaGrad.derive(relu[ops.Scalar])
    val hh = ops.innerAlgebra.elementWiseOpM(h, relu, dRelu)
    val o = hh * lastWs + lastW0.t
    val res = o.mapRows(row => stableSoftmax(ops)(row.t).t)
    res

def clip(ops: MatrixAlgebraT)(x: ops.Scalar)(using n: Numeric[ops.Scalar]): ops.Scalar =
    val epsilon: Double = 1e-07
    val minS = ops.liftToScalar(epsilon)
    val maxS = ops.liftToScalar(1.0 - epsilon)
    if n.lt(x, minS) then minS
    else if n.gt(x, maxS) then maxS
    else x

def crossEntropy(ops: MatrixAlgebraT)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Scalar =
    import ops.*
    val logYsHat = ysHat.map(clip(ops)).map(trig.log)
    val logYsHatYs = logYsHat *:* ys
    ops.liftToScalar(-1) * ops.divideSS(logYsHatYs.sum, ops.liftToScalar(logYsHat.nRows))

def cycle[T](seq: Seq[T]): LazyList[T] = {
    def inner(s: Seq[T]): LazyList[T] = s match {
        case head +: tail => head #:: inner(tail)
        case _            => inner(seq)
    }
    inner(seq)
}

val breezeOps = BreezeMatrixAlgebraT
def miniBatchGradientDescent
(data: LazyList[(breezeOps.Matrix, breezeOps.Matrix)])
(
    firstW0: breezeOps.ColumnVector,
    firstWs: breezeOps.Matrix,
    lastW0: breezeOps.ColumnVector,
    lastWs: breezeOps.Matrix,
    alpha: breezeOps.Scalar, 
    n: Int,
    verbose: Boolean = false
)(
    createDLoss: (breezeOps.Matrix, breezeOps.Matrix) => 
        ((breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix)) =>
        (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix)
)(using Numeric[breezeOps.Scalar], Trig[breezeOps.Scalar]): (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix) =
    if n == 0 then (firstW0, firstWs, lastW0, lastWs)
    else
        if (verbose && n % 100 == 0) println("Running iteration " + n)
        val (xsBatch, ysBatch) = data.head
        val dLoss = createDLoss(xsBatch, ysBatch)
        val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
        
        import breezeOps.*
        miniBatchGradientDescent(data.tail)(
            firstW0 - alpha * dFirstW0,
            firstWs - alpha * dFirstWs,
            lastW0 - alpha * dLastW0,
            lastWs - alpha * dLastWs, 
            alpha,
            n - 1,
            verbose
        )(createDLoss)

def gradientDescent(ops: MatrixAlgebraT)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
    alpha: ops.Scalar, 
    n: Int
)(
    // TODO fix that dLoss does not have to be tupled...
    dLoss: ((ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix)) => (ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix)
): (ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix) =
    if n == 0 then (firstW0, firstWs, lastW0, lastWs)
    else
        val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
        
        gradientDescent(ops)(
            firstW0 - alpha * dFirstW0,
            firstWs - alpha * dFirstWs,
            lastW0 - alpha * dLastW0,
            lastWs - alpha * dLastWs, 
            alpha,
            n - 1
        )(dLoss)

def lossF(ops: DualMatrixAlgebraT)(xs: ops.Matrix, ys: ops.Matrix)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using 
    Numeric[ops.Scalar], 
    Numeric[ops.PrimaryScalar], 
    Trig[ops.Scalar],
    DeriverFromTo[ops.Scalar => ops.Scalar, ops.PrimaryScalar => ops.PrimaryScalar],
): ops.Scalar =
    val ysHat = neuralNetwork(ops)(xs, firstW0, firstWs, lastW0, lastWs)
    crossEntropy(ops)(ys, ysHat)

def accuracy(yHatProp: DenseMatrix[Double], yM: DenseMatrix[Double]): Double =
    import breeze.linalg.{Vector => BreezeVector, *}
    val yHat = yHatProp(*, ::).map(x => argmax(x))
    val y = yM(*, ::).map(x => argmax(x))
    val correct = yHat.toArray.zip(y.toArray).map((yHat, y) => if yHat == y then 1 else 0).sum
    correct.toDouble / yHat.length

def makePredictions(xs: DenseMatrix[Double])(
    firstW0: DenseVector[Double],
    firstWs: DenseMatrix[Double],
    lastW0: DenseVector[Double],
    lastWs: DenseMatrix[Double],
): DenseMatrix[Double] =
    neuralNetworkApply(BreezeMatrixAlgebraT)(
        xs, firstW0, firstWs, lastW0, lastWs
    )

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

@main def neuralNetworkMNISTVectorAlg() = 

    val batchSize = 8
    val nFeatures = MNISTDataSet.nFeatures
    val nHiddenUnits = 36
    val nOutputUnits = MNISTDataSet.nLabels

    val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
    val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, batchSize)

    val rand = scala.util.Random(42)
    
    val (initFirstW0, initFirstWs, initLastW0, initLastWs) = getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits)

    val eagerData = cycle(xsTrain.zip(ysTrain).toList)

    val (xsM, ysM) = eagerData.head

    time {
        val initYsHatM = makePredictions(xsM)(initFirstW0, initFirstWs, initLastW0, initLastWs)
        println(f"${crossEntropy(BreezeMatrixAlgebraT)(ysM, initYsHatM)}  -- with initial weights")
        // println(f"${lossF(BreezeMatrixAlgebraT)(xsM, ysM)(initFirstW0, initFirstWs, initLastW0, initLastWs)}  -- with initial weights")
        println("train acc: " + accuracy(initYsHatM, ysM) * 100 + " -- with initial weights")
    }
    time {
        val iters = 1
        println(f"Start forward-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeMatrixAlgebraT)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan
        import DeriverBreezeForwardPlan.given
        val dLoss = ScalaGrad.derive(lossF(DeriverBreezeForwardPlan.algebraT)(
            DualNumberMatrix(xsM, DenseMatrix.zeros[Double](xsM.rows, xsM.cols)),
            DualNumberMatrix(ysM, DenseMatrix.zeros[Double](ysM.rows, ysM.cols))
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeMatrixAlgebraT)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }
    time {
        val iters = 1 // 1000 / batchSize
        println(f"Start reverse-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeMatrixAlgebraT)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan
        import DeriverBreezeReversePlan.given
        import scalagrad.api.reverse.dual.DualDeltaMatrix
        import scalagrad.api.reverse.delta.DeltaMatrix
        val dLoss = ScalaGrad.derive(lossF(DeriverBreezeReversePlan.algebraT)(
            DualDeltaMatrix(xsM, DeltaMatrix.Zero()),
            DualDeltaMatrix(ysM, DeltaMatrix.Zero())
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeMatrixAlgebraT)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }


@main def neuralNetworkMNISTVectorAlgPerformance() = 
    case class PerformanceTest(
        mode: String,
        batchSize: Int,
        samples: Int,
    )

    object PerformanceTest:
        def create(mode: String, batchSizes: List[Int], samples: Int): List[PerformanceTest] =
            batchSizes.map(batchSize => PerformanceTest(mode, batchSize, samples))

    val nFeatures = MNISTDataSet.nFeatures
    val nHiddenUnits = 36
    val nOutputUnits = MNISTDataSet.nLabels

    val (initFirstW0, initFirstWs, initLastW0, initLastWs) = getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits)

    println("****** START PERFORMANCE TESTS ******")
    val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, 32)
    val batchSizesReverseMode = List(64, 128)
    // val batchSizesReverseMode = List(8, 16, 32, 64, 128, 256, 512)
    val batchSizesForwardMode = List(8)
    //val batchSizesForwardMode = List(1, 2, 4, 8)
    
    for {
        test <-
            PerformanceTest.create("forward", batchSizesForwardMode, 1)
            ++ PerformanceTest.create("reverse", batchSizesReverseMode, 60_000)
    } {
            val (mode, batchSize, samples) = (test.mode, test.batchSize, test.samples)
            val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
            val eagerData = cycle(xsTrain.zip(ysTrain).toList)

            val iters: Int = Math.max(samples / batchSize, 1)

            val miniBatchGradientDescentF = miniBatchGradientDescent
                (eagerData)
                (initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters)

            val createDLoss = mode match {
                case "forward" => {
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(DeriverBreezeForwardPlan.algebraT)(
                            DualNumberMatrix(xs, DenseMatrix.zeros[Double](xs.rows, xs.cols)),
                            DualNumberMatrix(ys, DenseMatrix.zeros[Double](ys.rows, ys.cols))
                        ))
                    createDLoss
                }
                case "reverse" => {
                    import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan
                    import DeriverBreezeReversePlan.given
                    import scalagrad.api.reverse.dual.DualDeltaMatrix
                    import scalagrad.api.reverse.delta.DeltaMatrix
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(DeriverBreezeReversePlan.algebraT)(
                            DualDeltaMatrix(xs, DeltaMatrix.Zero()),
                            DualDeltaMatrix(ys, DeltaMatrix.Zero())
                        ))
                    createDLoss
                }
            }

            val ((firstW0, firstWs, lastW0, lastWs), trainTime) = timeMeasure({
                miniBatchGradientDescentF(createDLoss)
            }, unit = java.util.concurrent.TimeUnit.MILLISECONDS)
            val ysHatTest = xsTest.map(xs => makePredictions(xs)(firstW0, firstWs, lastW0, lastWs))
            val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
            val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
            val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(BreezeMatrixAlgebraT)(ys, ysHat)).toList
            val lossTest = lossTestBatch.sum / lossTestBatch.length

            val trainTimeNormalized = trainTime * (60_000d / iters / batchSize) 

            println(
                List(
                    f"mode=$mode%15s",
                    f"batch_size=${batchSize}%3d",
                    f"iters=${iters}%5d",
                    f"testLoss=${lossTest}%.1f",
                    f"testAcc=${accuracyTest * 100}%3f",
                    f"trainTime=${trainTime / 1000}%3.1fs",
                    f"trainTime/60_000≈${trainTimeNormalized / 1000}%10.2fs",
                ).mkString("\t")
            )
            Thread.sleep(5000) // to avoid overheating and give GC some time to clean up memory
    }