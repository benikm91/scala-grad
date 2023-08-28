package scalagrad.showcase.probabilisticprogramming.distribution

case class UnnormalizedDistribution[Sample](private val f: Sample => Double):
    def apply(sample: Sample): Double = f(sample)