package scalagrad.showcase.probabilisticprogramming.metropolisHastings

import scalagrad.showcase.probabilisticprogramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposerConditionalDistribution.ProposerConditionalDistribution

import scala.math.exp


trait MetropolisHastingsSampler extends MetropolisSampler:

    val pcd: ProposerConditionalDistribution[Sample]

    override def acceptancePropability(target: UnnormalizedDistribution[Sample] | UnnormalizedLogDistribution[Sample], proposedSample: Sample, currentSample: Sample): Double = 
         target match
            case target: UnnormalizedLogDistribution[Sample] => 
                val pp = target(proposedSample) - target(currentSample)
                val gg = pcd.g(currentSample, proposedSample) - pcd.g(proposedSample, currentSample)
                exp(pp + gg)
            case target: UnnormalizedDistribution[Sample] =>    
                val pp = target(proposedSample) / target(currentSample)
                val gg = pcd.g(currentSample, proposedSample) / pcd.g(proposedSample, currentSample)
                pp * gg