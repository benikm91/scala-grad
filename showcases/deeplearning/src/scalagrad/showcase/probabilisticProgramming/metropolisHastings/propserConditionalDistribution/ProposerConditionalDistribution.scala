package scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposerConditionalDistribution


trait ProposerConditionalDistribution[Sample]:
    def g(to: Sample, from: Sample): Double
