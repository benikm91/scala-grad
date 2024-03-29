package scalagrad.showcase.probabilisticprogramming.metropolisHastings

import scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer.GaussianProposer

import scala.util.Random



case class GaussianMetropolisSampler(
    rng: Random, 
    stepSize: Double,
) extends MetropolisSampler:
    override type Sample = Vector[Double]
    override val uniform = ScalaUniformRandomGenerator(rng)
    override val proposer = GaussianProposer(stepSize)

    def showHyperParams: String = "stepSize = " + stepSize
