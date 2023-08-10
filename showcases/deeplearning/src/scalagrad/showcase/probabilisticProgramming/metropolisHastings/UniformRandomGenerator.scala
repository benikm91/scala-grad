package scalagrad.showcase.probabilisticProgramming.metropolisHastings

import scala.util.Random


trait UniformRandomGenerator:
    def nextDouble(): Double

case class ScalaUniformRandomGenerator(rng: Random) extends UniformRandomGenerator:
    def nextDouble(): Double = rng.nextDouble
