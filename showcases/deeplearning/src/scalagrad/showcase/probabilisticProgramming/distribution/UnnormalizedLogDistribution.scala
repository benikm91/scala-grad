package scalagrad.showcase.probabilisticProgramming.distribution

case class UnnormalizedLogDistribution[Sample](private val f: Sample => Double):
    def apply(sample: Sample): Double = f(sample)
