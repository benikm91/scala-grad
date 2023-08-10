package scalagrad.showcase.probabilisticProgramming.metropolisHastings

import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposerConditionalDistribution.ProposerConditionalDistribution
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