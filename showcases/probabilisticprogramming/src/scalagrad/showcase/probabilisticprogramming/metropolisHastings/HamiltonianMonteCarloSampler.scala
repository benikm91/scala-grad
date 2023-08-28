package scalagrad.showcase.probabilisticprogramming.metropolisHastings

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import scalagrad.showcase.probabilisticprogramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer.{HamiltonianDynamicsLeapFrogProposer, Proposer}

import scala.math.{exp, min, sqrt}
import scala.util.Random

case class HamiltonianMonteCarloSampler(
    rng: Random,
    dTarget: Vector[Double] => Vector[Double],
    stepSize: Double,
    l: Int,
) 
extends MetropolisSampler:
    type Sample = (Vector[Double], Vector[Double])
    override val uniform = ScalaUniformRandomGenerator(rng)
    override val proposer = HamiltonianDynamicsLeapFrogProposer(
        dTarget,
        stepSize,
        l,
    )

    def generateRandomMomentum(size: Int): Vector[Double] = 
        Proposer.gaussianDiagSample(Vector.fill(size)(0), 1.0)

    override def nextSample(target: Target)(currentSample : Sample): Sample =
        val nextSample = super.nextSample(target)(currentSample)
        (
            nextSample._1, 
            // overwrite momentum to gaussian (Hamiltonian implementation)
            generateRandomMomentum(nextSample._2.size)
        )

    def apply(
        target: UnnormalizedLogDistribution[Vector[Double]],
        init: Vector[Double],
    ): Iterator[Vector[Double]] = 
        def constructFullTarget(target: UnnormalizedLogDistribution[Vector[Double]]): UnnormalizedLogDistribution[(Vector[Double], Vector[Double])] = 
            def targetK(p: Vector[Double]) = 
                -0.5 * (p.map(x => x * x).sum + p.size * math.log(2 * math.Pi))
            UnnormalizedLogDistribution(
                (q, p) =>
                    val u = target(q)
                    val k = targetK(p)
                    u + k
            )
        apply(constructFullTarget(target), (init, generateRandomMomentum(init.size)))
            .map(_._1)

    def showHyperParams: String = "stepSize = " + stepSize + ", l = " + l
