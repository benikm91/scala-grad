package scalagrad.showcase.probabilisticprogramming

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import breeze.stats.meanAndVariance
import scalagrad.api.forward.ForwardMode.derive as d
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
import scalagrad.showcase.probabilisticprogramming.distribution.UnnormalizedLogDistribution
import scalagrad.showcase.probabilisticprogramming.metropolisHastings.HamiltonianMonteCarloSampler
import spire.compat.numeric
import spire.implicits.*

import scala.util.Random

/**
 * This example shows a simple bayesian linear regression model with two parameters
 * We first formulate a posterior and then sample it with the gradient-based hamiltonial monte carlo sampler (HMC).
 * For calculating the gradients ScalaGrad is used.
 */
object BayesianLinearRegression extends App:

    // Target configuration and data generation
    val (a, b, sigma) = (0.2, 3, 0.5)
    val errorDist = Gaussian(0, sigma)
    val data = for (x <- 0 until 100) yield {
        (x, a * x + b + errorDist.draw())
    }

    // Implementing a gaussian distribution with MastrixAlgebra to make it derivable by ScalaGrad.
    case class GaussianDerivable[S](mean: S, sigma: S)(using alg: MatrixAlgebra[S, _, _, _]):
        def logPdf(x: S): S =
            val res = (x - mean) / sigma
            val normalizationTerm = alg.num.sqrt(alg.lift(2 * Math.PI)) * sigma
            alg.lift(-0.5) * res * res - alg.trig.log(normalizationTerm)

    // Formulating the posterior
    def logPosterior(alg: MatrixAlgebraDSL)(params: alg.ColumnVector): alg.Scalar =
        logPrior(alg)(params) + logLikelihood(alg)(params)

    // Formulating the prior
    def logPrior(alg: MatrixAlgebraDSL)(params: alg.ColumnVector): alg.Scalar =
        def logPriorDistA(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.lift(0), alg.lift(1)).logPdf(x)
        def logPriorDistB(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.lift(0), alg.lift(10)).logPdf(x)
        def logPriorDistSigma(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.lift(0), alg.lift(0.25)).logPdf(x)
        val (a, b, sigma) = (params.elementAt(0), params.elementAt(1), params.elementAt(2))
        logPriorDistA(alg)(a) + logPriorDistB(alg)(b) + logPriorDistSigma(alg)(sigma)

    // Formulating the liklihood
    def logLikelihood(alg: MatrixAlgebraDSL)(params: alg.ColumnVector): alg.Scalar =
        val (a, b, sigma) = (params.elementAt(0), params.elementAt(1), params.elementAt(2))
        data.map((x, y) => 
            val gaussian = GaussianDerivable(a * alg.lift(x) + b, sigma)
            gaussian.logPdf(alg.lift(y))
        ).sum

    // Derive the posterior for HMC
    val dLogPosterior = d(logPosterior)(BreezeDoubleMatrixAlgebraDSL)
    
    // Initialize HMC sampler and sample the posterior
    lazy val hamiltonianSamples = 
        HamiltonianMonteCarloSampler(
            new Random(),
            v => dLogPosterior(DenseVector(v.toArray)).toScalaVector,
            stepSize = 1e-3, 
            l = 20,
        )
            .apply(UnnormalizedLogDistribution[Vector[Double]](v => logPosterior(BreezeDoubleMatrixAlgebraDSL)(DenseVector(v.toArray))), Vector(0.0, 0.0, 1.0))
            .drop(200)
            .take(200).toSeq

    val samples = hamiltonianSamples

    // Output results

    println("Hamiltonian Monte Carlo")
    val meanAndVarianceA = meanAndVariance(samples.map(_(0)))
    println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
    val meanAndVarianceB = meanAndVariance(samples.map(_(1)))
    println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
    val meanAndVariancesigma = meanAndVariance(samples.map(_(2)))
    println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
    println("***")
