package scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposerConditionalDistribution


trait ProposerConditionalDistribution[Sample]:
    def g(to: Sample, from: Sample): Double
