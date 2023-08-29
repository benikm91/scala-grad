package scalagrad.showcase.deeplearning.mnist

import scalagrad.showcase.deeplearning.mnist.MNISTDataSet
import breeze.linalg.{DenseMatrix, DenseVector}
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import spire.algebra.Trig
import spire.math.Numeric
import spire.syntax.all.partialOrderOps

object Util:

    /** Cycle seq infinitely. */
    def cycle[T](seq: Seq[T]): LazyList[T] =
        def inner(s: Seq[T]): LazyList[T] = s match {
            case head +: tail => head #:: inner(tail)
            case _            => inner(seq)
        }
        inner(seq)

    /**
     * Preprocess an MNIST dataset.
     * - Normalize pixels to be in [0, 1].
     * - Convert labels to one-hot encoding.
     * - Batch data into batches of size batchSize.
     * @return (xsBatches, ysBatches) where xsBatches is a LazyList of batches of input data and ysBatches is a LazyList of batches of labels.
     */
    def preprocess(data: Iterator[MNISTDataSet.MNISTEntry], batchSize: Int) = 
        def normalize(pixel: Double) = pixel / 255
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
