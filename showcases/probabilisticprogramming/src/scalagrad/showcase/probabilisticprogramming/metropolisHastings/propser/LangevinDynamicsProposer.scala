package scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis

import scala.math.sqrt


case class LangevinDynamicsProposer(dTarget: Vector[Double] => Vector[Double], stepSize: Double, sigma: Double) extends Proposer[Vector[Double]]:

    override def nextProposal(x: Vector[Double]) =
        val step = dTarget(x).map(_ * stepSize)
        val gaussianNoise = Proposer.gaussianDiagSample(Vector.fill(x.size)(0), 1.0)
        val noiseF = sqrt(2 * stepSize)
        val noise = gaussianNoise.map(_ * noiseF)
        x.zip(step).map(_ + _).zip(noise).map(_ + _)
