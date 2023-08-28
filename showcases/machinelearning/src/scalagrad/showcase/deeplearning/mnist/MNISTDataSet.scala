package scalagrad.showcase.deeplearning.mnist

import scala.io.Source

object MNISTDataSet:

    val nLabels = 10
    val nFeatures = 28 * 28
    val testSize = 10000
    val trainSize = 60000

    case class MNISTEntry(
        label: Int,
        pixels: Seq[Double]
    )
 
    def loadTest: Iterator[MNISTEntry] = 
        val content = Source.fromFile("showcases/machinelearning/res/mnist/mnist_test.csv")
            .getLines.map(_.split(","))
        content.drop(1).map(row =>
            MNISTEntry(
                row.head.toInt,
                row.tail.map(_.toDouble)
            )
        )

    def loadTrain: Iterator[MNISTEntry] = 
        val content = Source.fromFile("showcases/machinelearning/res/mnist/mnist_train.csv")
            .getLines.map(_.split(","))
        content.drop(1).map(row =>
            MNISTEntry(
                row.head.toInt,
                row.tail.map(_.toDouble)
            )
        )