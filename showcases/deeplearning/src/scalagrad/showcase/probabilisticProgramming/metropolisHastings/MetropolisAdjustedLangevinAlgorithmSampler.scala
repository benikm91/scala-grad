package scalagrad.showcase.probabilisticProgramming.metropolisHastings

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposer.LangevinDynamicsProposer
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposerConditionalDistribution.LangevinDynamicsProposerConditionalDistribution

import scala.math.{exp, min, sqrt}
import scala.util.Random

case class MetropolisAdjustedLangevinAlgorithmSampler(
    rng: Random,
    dTarget: Vector[Double] => Vector[Double],
    stepSize: Double,
    sigma: Double
) 
extends MetropolisHastingsSampler:
    type Sample = Vector[Double]
    override val uniform = ScalaUniformRandomGenerator(rng)
    override val proposer = LangevinDynamicsProposer(dTarget, stepSize, sigma)
    override val pcd = LangevinDynamicsProposerConditionalDistribution(dTarget, stepSize)

    def showHyperParams: String = "stepSize = " + stepSize + ", sigma = " + sigma
