package scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposerConditionalDistribution


case class LangevinDynamicsProposerConditionalDistribution(dTarget: Vector[Double] => Vector[Double], stepSize: Double) extends ProposerConditionalDistribution[Vector[Double]]:
    override def g(x: Vector[Double], cx: Vector[Double]): Double =
        val step = dTarget(x).map(_ * stepSize)
        val v = x.zip(cx).map(_ - _).zip(step).map(_ - _)
        val vsq = v.zip(v).map(_ * _).reduce(_ + _)
        -0.25 * stepSize * vsq
