package scalagrad.showcase.probabilisticProgramming

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import spire.math.Numeric
import spire.implicits._
import spire.algebra.Trig
import spire.compat.numeric

import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.GaussianMetropolisSampler
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.MetropolisAdjustedLangevinAlgorithmSampler
import scala.util.Random

import scalagrad.api.forward.ForwardMode.derive as d

import breeze.stats.meanAndVariance
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
import scalagrad.api.forward.ForwardDualMode
import scala.reflect.api.Mirror

object UseCase1 extends App:

    def d(
        f: (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => alg.Scalar
    ):(alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar, alg.Scalar) = 
        alg => (s1, s2, s3) =>
            val mode = ForwardDualMode(alg.innerAlgebra)
            val df = mode.deriveDualTuple2Scalar(f(mode.algebraDSL).tupled)
            df((s1, s2, s3))

    // Target configuration and data generation
    val a = 0.2
    val b = 3
    val sigma = 0.5
    val errorDist = Gaussian(0, sigma)
    val data = for (x <- 0 until 100) yield {
        (x, a * x + b + errorDist.draw())
    }

    case class Parameters[T](a : T, b:  T, sigma : T)
    
    object Parameters:
        extension [T] (p: Parameters[T])
            def toVector: Vector[T] = Vector(p.a, p.b, p.sigma)
        def fromVector[T](v: Vector[T]): Parameters[T] = Parameters[T](v(0), v(1), v(2))

    case class GaussianDerivable[S](alg: MatrixAlgebra[S, _, _, _], mean: S, sigma: S):
        import alg.*
        def logPdf(x: S): S =
            val res = (x - mean) / sigma
            val normalizationTerm = alg.num.sqrt(alg.lift(2 * Math.PI)) * sigma
            alg.lift(-0.5) * res * res - alg.trig.log(normalizationTerm)

    def logPrior(alg: MatrixAlgebraDSL)(p: Parameters[alg.Scalar]): alg.Scalar =
        def logPriorDistA(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(1)).logPdf(x)
        def logPriorDistB(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(10)).logPdf(x)
        def logPriorDistSigma(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
            GaussianDerivable(alg.innerAlgebra, alg.lift(0), alg.lift(0.25)).logPdf(x)
        logPriorDistA(alg)(p.a) + logPriorDistB(alg)(p.b) + logPriorDistSigma(alg)(p.sigma)

    def logLikelihood(alg: MatrixAlgebraDSL)(p: Parameters[alg.Scalar]): alg.Scalar =
        data.map((x, y) => 
            val gaussian = GaussianDerivable(alg.innerAlgebra, p.a * alg.lift(x) + p.b, p.sigma)
            gaussian.logPdf(alg.lift(y))
        ).sum

    def logPosterior(alg: MatrixAlgebraDSL)(a : alg.Scalar, b:  alg.Scalar, sigma : alg.Scalar): alg.Scalar =
        val p = Parameters(a, b, sigma)
        logPrior(alg)(p) + logLikelihood(alg)(p)
     
    val stepSize = 0.1
    val dLogPosterior = d(logPosterior)(BreezeDoubleMatrixAlgebraDSL)
    
    // Metropolis samples
    lazy val metroSamples =
        GaussianMetropolisSampler(
            new Random(),
            stepSize, 
        )
            .apply(UnnormalizedLogDistribution(v => logPosterior(BreezeDoubleMatrixAlgebraDSL)(v(0), v(1), v(2))), Vector(0.0, 0.0, 1.0))
            .drop(5_000)
            .take(10_000).toSeq

    // MALA samples
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

    // Output results
    {
        println("Metro")
        val samples = metroSamples.map(Parameters.fromVector(_))

        val meanAndVarianceA = meanAndVariance(samples.map(_.a))
        println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
        val meanAndVarianceB = meanAndVariance(samples.map(_.b))
        println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
        val meanAndVariancesigma = meanAndVariance(samples.map(_.sigma))
        println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
        println("***")
    }
    {
        val samples = malaSamples.map(Parameters.fromVector(_))

        println("MALA")
        val meanAndVarianceA = meanAndVariance(samples.map(_.a))
        println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
        val meanAndVarianceB = meanAndVariance(samples.map(_.b))
        println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
        val meanAndVariancesigma = meanAndVariance(samples.map(_.sigma))
        println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
        println("***")
        
        println(f"End Gradient: ${d(logPosterior)(BreezeDoubleMatrixAlgebraDSL)(meanAndVarianceA.mean, meanAndVarianceB.mean, meanAndVariancesigma.mean)}")
    }
