package scalagrad.showcase.probabilisticProgramming

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import spire.math.Numeric
import spire.implicits._
import spire.algebra.Trig
import spire.compat.numeric

import scalagrad.api.matrixalgebra.MatrixAlgebraT

import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given
import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode.given
import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode.{algebraT => algebra}

import scalagrad.api.ScalaGrad
import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.GaussianMetropolisSampler
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.MetropolisAdjustedLangevinAlgorithmSampler
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.HamiltonianMonteCarloSampler
import scala.util.Random

import scaltair.*
import scaltair.PlotTargetBrowser.given

import breeze.stats.meanAndVariance
import breeze.linalg.{DenseVector, DenseMatrix}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraT

object UseCase1b extends App:
    // config
    val numWarmup = 5_000
    val numSamples = 1_000
    val numDataPoints = 100

    // (unknown) target variables
    val a = Vector(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)  // coefficients for each feature
    val b = 2
    val sigma = 0.5
    val errorDist = Gaussian(0, sigma)

    // synthetic data
    val (xs, ys) = {
        val data = for (i <- 0 until numDataPoints) yield {
            val xs = Vector.fill(a.size)(Random.nextDouble() * 2 - 1)
            val y = xs.zip(a).map(_ * _).sum + b + errorDist.draw()
            (xs, y)
        }
        val xs = DenseMatrix(data.map(_._1.toArray): _*)
        val ys = DenseVector(data.map(_._2).toArray)
        (xs, ys)
    }

    def logLikelihood(alg: MatrixAlgebraT)(xs: alg.Matrix, ys: alg.ColumnVector)(a: alg.ColumnVector, b: alg.Scalar, sigma: alg.Scalar)
        (using num: Numeric[alg.Scalar], trig: Trig[alg.Scalar]): alg.Scalar =
        xs.rows.zip(ys.elements).map { case (x, y) =>
            val mu = (x * a) + b
            val diff = y - mu
            val exponent = alg.liftToScalar(-0.5) * (diff * diff) / (sigma * sigma)
            val normalization = alg.liftToScalar(1.0) / (alg.liftToScalar(sqrt(2 * scala.math.Pi)) * sigma)
            trig.log(normalization) + exponent
        }.sum

    def logPrior(alg: MatrixAlgebraT)(a: alg.ColumnVector, b: alg.Scalar, sigma: alg.Scalar)
        (using num: Numeric[alg.Scalar], trig: Trig[alg.Scalar]): alg.Scalar =
        def logPriorDistA[T: Trig](x: T)(using num: Numeric[T]) = 
            gaussianLogPdf[T](num.zero, num.fromInt(1))(x)
        def logPriorDistB[T: Trig](x: T)(using num: Numeric[T]) = 
            gaussianLogPdf[T](num.zero, num.fromInt(10))(x)
        def logPriorDistSigma[T](x: T)(using num: Numeric[T], trig: Trig[T]) = 
            logNormalLogPdf[T](num.zero, num.fromDouble(0.25))(trig.log(x))
        a.map(logPriorDistA(_)).sum + logPriorDistB(b) + logPriorDistSigma(sigma)

    def logPosterior(alg: MatrixAlgebraT)(xs: alg.Matrix, ys: alg.ColumnVector)(a: alg.ColumnVector, b: alg.Scalar, sigma: alg.Scalar)
        (using num: Numeric[alg.Scalar], trig: Trig[alg.Scalar]): alg.Scalar =
        logLikelihood(alg)(xs, ys)(a, b, sigma) + logPrior(alg)(a, b, sigma)

    def paramsToVector(a: DenseVector[Double], b: Double, sigma: Double): Vector[Double] = a.toScalaVector ++ Seq(b, sigma)
    def vectorToParams(v: Vector[Double]): (DenseVector[Double], Double, Double) = (DenseVector(v.take(10).toArray), v(10), v(11))

    // derive logPosterior for MALA and Hamiltonian Monte Carlo
    val dLogPosterior = ScalaGrad.derive(logPosterior(algebra)(algebra.lift(xs), algebra.lift(ys)))
    val dLogPosteriorVector = vectorToParams andThen dLogPosterior andThen paramsToVector
    
    // bring logPosterior into a form that can be used by the samplers
    def logPosteriorDouble(a: DenseVector[Double], b: Double, sigma: Double) =
        logPosterior(BreezeDoubleMatrixAlgebraT)(xs, ys)(a, b, sigma)
    val logPosteriorDoubleVector = vectorToParams andThen logPosteriorDouble

    // define initial parameter setting
    val initParams: Vector[Double] = {
        val (initA, initB, initSigma) = (DenseVector.fill(10)(0.0), 0.0, 1.0)
        paramsToVector(initA, initB, initSigma)
    }
    
    // sample posterior by metropolitan algorithm
    lazy val metroSamples =
        GaussianMetropolisSampler(
            new Random(),
            stepSize = 1.0 / numDataPoints, 
        )
            .apply(UnnormalizedLogDistribution(logPosteriorDoubleVector), initParams)
            .drop(numWarmup)
           .take(numSamples).toSeq

    // sample posterior by MALA
    lazy val malaSamples = 
        MetropolisAdjustedLangevinAlgorithmSampler(
            new Random(),
            dLogPosteriorVector,
            stepSize = 1e-3 / numDataPoints, 
            sigma = 1.0
        )
            .apply(UnnormalizedLogDistribution(logPosteriorDoubleVector), initParams)
            .drop(numWarmup)
            .take(numSamples).toSeq

    // sample posterior by Hamiltonian Monte Carlo
    lazy val hamiltonianSamples = 
        HamiltonianMonteCarloSampler(
            new Random(),
            dLogPosteriorVector,
            stepSize = 1e-1 / numDataPoints, 
            l = 20,
        )
            .apply(UnnormalizedLogDistribution(logPosteriorDoubleVector), initParams)
            .drop(numWarmup)
            .take(numSamples).toSeq

    // Visualize samples with scaltair

    def plotSamples(samples: Seq[Vector[Double]], title: String): Unit =
        def calcAcceptanceRate(samples: Seq[Seq[Double]]): Double = 
            val totalSamples = samples.size
            val sameSamples = samples.sliding(2).count { case Seq(v1, v2) => v1 == v2 }
            1.0 - (sameSamples.toDouble / totalSamples)

        val ar = calcAcceptanceRate(samples.map(vectorToParams(_)._1.toScalaVector))

        val longDf = samples.zipWithIndex.flatMap { case (sample, i) =>
            val (a, b, sigma) = vectorToParams(sample)
            a.toScalaVector.zipWithIndex.map { case (a, j) => s"a$j" -> a } 
                ++ Map("b" -> b) ++ Map("sigma" -> sigma)
        }
        
        // Stupid way to get the true values in the same format to combine them into the same plot (it ain't stupid if it works, right?)
        val data = Map(
            "key" -> longDf.map(_._1),
            "keyTrue" -> List.fill(samples.size)((0 until 10).map(i => s"a${i}").toList ++ List("b", "sigma")).flatten,
            "value" -> longDf.map(_._2),
            "valueTrue" -> List.fill(samples.size)(a.toList ++ List(b, sigma)).flatten,
            "type" -> longDf.map(_ => "sample"),
            "typeTrue" -> longDf.map(_ => "true"),
        )

        val boxPlot = Chart(data)
            .encode(
                Channel.X("value", FieldType.Quantitative),
                Channel.Y("key", FieldType.Nominal),
                Channel.Color("type", FieldType.Nominal),
            )
            .markBoxplot() 

        val trueValues = Chart(data)
            .encode(
                Channel.X("valueTrue", FieldType.Quantitative),
                Channel.Y("keyTrue", FieldType.Nominal),
                Channel.Color("typeTrue", FieldType.Nominal),
            )
            .markCircle()
            
        boxPlot.overlay(trueValues)
            .properties(
                ChartProperties(title=s"$title $numSamples samples with $numWarmup warmup and $numDataPoints data points, acceptance rate: ${Math.round(ar * 100)}%"),
            )
            .show()

    println("Metro")
    plotSamples( 
        metroSamples,
        title="Metro"
    )
    
    println("MALA")
    plotSamples(
        malaSamples,
        title="MALA"
    )
    
    println("DONE")
    
    println("Hamiltonian")
    plotSamples(
        hamiltonianSamples,
        title="Hamiltonian"
    )
    
    println("DONE")

    // mathematical functions

    def gaussianLogPdf[T: Numeric: Trig](mean: T, sigma: T)(x: T): T = {
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val pi = num.fromDouble(math.Pi)
        val two = num.fromInt(2)
        val sigma2 = sigma * sigma
        val diff = x - mean
        val diff2 = diff * diff
        -trig.log(two * pi * sigma2) / 2 - diff2 / (two * sigma2)
    } ensuring { res =>
        def ~=(x: Double, y: Double) = {
            if ((x - y).abs < 0.001) true else false
        }
        val num = summon[Numeric[T]]
        // println(f"${mean.toDouble}, ${sigma.toDouble}")
        // println(f"${res.toDouble} ~= ${Gaussian(mean.toDouble, sigma.toDouble).logPdf(x.toDouble)}")
        true || ~=(res.toDouble, Gaussian(mean.toDouble, sigma.toDouble).logPdf(x.toDouble))
    }

    def logNormalLogPdf[T: Numeric: Trig](mean: T, sigma: T)(x: T): T =
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val pi = num.fromDouble(math.Pi)
        gaussianLogPdf(mean, sigma)(x)