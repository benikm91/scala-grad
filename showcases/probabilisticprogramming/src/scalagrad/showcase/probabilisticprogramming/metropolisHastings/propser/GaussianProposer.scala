package scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis


case class GaussianProposer(stepSize: Double) extends Proposer[Vector[Double]]:
    def nextProposal(x : Vector[Double]): Vector[Double] =
        val mvn = MultivariateGaussian(
            DenseVector(x.toArray),
            DenseMatrix.eye[Double](x.size) * stepSize,
        )
        mvn.draw().toArray.toVector
