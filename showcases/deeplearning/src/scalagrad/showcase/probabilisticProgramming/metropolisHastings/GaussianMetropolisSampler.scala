package scalagrad.showcase.probabilisticProgramming.metropolisHastings

import scala.util.Random
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposer.GaussianProposer



case class GaussianMetropolisSampler(
    rng: Random, 
    stepSize: Double,
) extends MetropolisSampler:
    override type Sample = Vector[Double]
    override val uniform = ScalaUniformRandomGenerator(rng)
    override val proposer = GaussianProposer(stepSize)

    def showHyperParams: String = "stepSize = " + stepSize
