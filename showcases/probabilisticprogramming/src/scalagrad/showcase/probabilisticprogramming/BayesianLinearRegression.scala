package scalagrad.showcase.probabilisticprogramming

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import spire.math.Numeric
import spire.implicits._
import spire.algebra.Trig
import spire.compat.numeric

import scalagrad.showcase.probabilisticprogramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticprogramming.metropolisHastings.GaussianMetropolisSampler
import scalagrad.showcase.probabilisticprogramming.metropolisHastings.MetropolisAdjustedLangevinAlgorithmSampler
import scala.util.Random

import scalagrad.api.forward.ForwardMode.derive as d

import breeze.stats.meanAndVariance
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
import scalagrad.api.forward.ForwardDualMode
import scala.reflect.api.Mirror

/**
 * This example shows a simple bayesian linear regression model with two parameters
 * We first formulate a posterior and then sample it with the gradient-based metropolis djusted langevin algorithm sampler (MALA).
 * For calculating the gradients ScalaGrad is used.
 */
object BayesianLinearRegression extends App:

    // Helper function taking care of the types
    def d(
        f: (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => alg.Scalar
    ):(alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar, alg.Scalar) = 
        alg => (s1, s2, s3) =>
            val mode = ForwardDualMode(alg.innerAlgebra)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df((s1, s2, s3))

    // Target configuration and data generation
    val (a, b, sigma) = (0.2, 3, 0.5)
    val errorDist = Gaussian(0, sigma)
    val data = for (x <- 0 until 100) yield {
        (x, a * x + b + errorDist.draw())
    }

    // Group parameters of model in case class
    case class Parameters[T](a : T, b:  T, sigma : T)
    
    object Parameters:
        extension [T] (p: Parameters[T])
            def toVector: Vector[T] = Vector(p.a, p.b, p.sigma)
        def fromVector[T](v: Vector[T]): Parameters[T] = Parameters[T](v(0), v(1), v(2))

    // Implementing a gaussian distribution with MastrixAlgebra to make it derivable by ScalaGrad.
    case class GaussianDerivable[S](alg: MatrixAlgebra[S, _, _, _], mean: S, sigma: S):
        import alg.*
        def logPdf(x: S): S =
            val res = (x - mean) / sigma
            val normalizationTerm = alg.num.sqrt(alg.lift(2 * Math.PI)) * sigma
            alg.lift(-0.5) * res * res - alg.trig.log(normalizationTerm)

    // Formulating the prior
    def logPrior(alg: MatrixAlgebraDSL)(p: Parameters[alg.Scalar]): alg.Scalar =
        def logPriorDistA(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(1)).logPdf(x)
        def logPriorDistB(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(10)).logPdf(x)
        def logPriorDistSigma(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(0.25)).logPdf(x)
        logPriorDistA(alg)(p.a) + logPriorDistB(alg)(p.b) + logPriorDistSigma(alg)(p.sigma)

    // Formulating the liklihood
    def logLikelihood(alg: MatrixAlgebraDSL)(p: Parameters[alg.Scalar]): alg.Scalar =
        data.map((x, y) => 
            val gaussian = GaussianDerivable(alg.innerAlgebra, p.a * alg.lift(x) + p.b, p.sigma)
            gaussian.logPdf(alg.lift(y))
        ).sum

    // Combining prior and liklihood to the posterior
    def logPosterior(alg: MatrixAlgebraDSL)(a : alg.Scalar, b:  alg.Scalar, sigma : alg.Scalar): alg.Scalar =
        val p = Parameters(a, b, sigma)
        logPrior(alg)(p) + logLikelihood(alg)(p)

    // Derive the posterior for MALA
    val dLogPosterior = d(logPosterior)(BreezeDoubleMatrixAlgebraDSL)
    
    val stepSize = 0.1
    
    // Initialize MALA sampler and sample the posterior

    lazy val malaSamples = 
        MetropolisAdjustedLangevinAlgorithmSampler(
            new Random(),
            v => dLogPosterior(v(0), v(1), v(2)).toList.toVector,
            stepSize = 1e-6,
            sigma = 1.0
        )
            .apply(UnnormalizedLogDistribution(v => logPosterior(BreezeDoubleMatrixAlgebraDSL)(v(0), v(1), v(2))), Vector(0.0, 0.0, 1.0))
            .drop(50_000)
            .take(50_000).toSeq

    val samples = malaSamples.map(Parameters.fromVector(_))

    // Output results

    println("MALA")
    val meanAndVarianceA = meanAndVariance(samples.map(_.a))
    println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
    val meanAndVarianceB = meanAndVariance(samples.map(_.b))
    println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
    val meanAndVariancesigma = meanAndVariance(samples.map(_.sigma))
    println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
    println("***")
    
    println(f"End Gradient: ${d(logPosterior)(BreezeDoubleMatrixAlgebraDSL)(meanAndVarianceA.mean, meanAndVarianceB.mean, meanAndVariancesigma.mean)}")
