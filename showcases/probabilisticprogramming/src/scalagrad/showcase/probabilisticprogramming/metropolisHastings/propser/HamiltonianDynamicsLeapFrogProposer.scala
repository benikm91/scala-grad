package scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis

import scala.math.sqrt


case class HamiltonianDynamicsLeapFrogProposer(
    dTarget: Vector[Double] => Vector[Double],
    stepSize: Double, 
    l: Int,
) extends Proposer[(Vector[Double], Vector[Double])]:

    override def nextProposal(z: (Vector[Double], Vector[Double])) =
        def calcNextQ(q: Vector[Double], p: Vector[Double], stepSize: Double) = 
            q.zip(p).map((q, p) => q + stepSize * p)
        def calcNextP(q: Vector[Double], p: Vector[Double], stepSize: Double, factor: Double = 1.0) = 
            dTarget(q).map(_ * -1).zip(p).map((dq, p) => p - stepSize * dq * factor)
        
        val (q, currentP) = z
        val p = calcNextP(q, currentP, stepSize, factor = 0.5)
        lazy val samples: LazyList[(Vector[Double], Vector[Double])] = 
            (q, p) #:: samples.map((q, p) => 
                val nextQ = calcNextQ(q, p, stepSize)
                (
                    nextQ,
                    calcNextP(nextQ, p, stepSize)
                )
            )
        val (prevFinalQ, prevFinalP) = samples.drop(l - 1).head
        val finalQ = calcNextQ(prevFinalQ, prevFinalP, stepSize)
        val finalP = calcNextP(finalQ, prevFinalP, stepSize, factor = 0.5)
            .map(_ * -1)
        
        (finalQ, finalP)

