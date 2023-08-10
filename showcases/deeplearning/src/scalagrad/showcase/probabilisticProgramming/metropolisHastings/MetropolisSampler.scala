package scalagrad.showcase.probabilisticProgramming.metropolisHastings

import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposer.Proposer
import scala.math.{exp, min}

trait MetropolisSampler:

    type Sample
    val uniform: UniformRandomGenerator
    val proposer: Proposer[Sample]

    type Target = UnnormalizedDistribution[Sample] | UnnormalizedLogDistribution[Sample]

    def acceptancePropability(target: Target, proposedSample: Sample, currentSample: Sample): Double = 
        target match
            case target: UnnormalizedLogDistribution[Sample] => 
                exp(
                    target(proposedSample) - target(currentSample)
                )
            case target: UnnormalizedDistribution[Sample] =>
                target(proposedSample) / target(currentSample)
    
    def nextSample(target: Target)(currentSample : Sample) : Sample = {
        val proposedSample = proposer.nextProposal(currentSample)

        val alpha = min(1.0, acceptancePropability(target, proposedSample, currentSample))
        val r = uniform.nextDouble()
        if (r < alpha) then proposedSample else currentSample
    }

    def apply(target: Target, initialSample : Sample): Iterator[Sample] = 
        Iterator.iterate(initialSample)(nextSample(target))

