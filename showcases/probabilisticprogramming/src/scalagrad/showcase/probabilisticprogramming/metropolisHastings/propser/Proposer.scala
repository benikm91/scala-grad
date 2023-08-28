package scalagrad.showcase.probabilisticprogramming.metropolisHastings.proposer

trait Proposer[Sample]:
    def nextProposal(x: Sample): Sample

object Proposer:

    import breeze.linalg.{DenseMatrix, DenseVector}
    import breeze.stats.distributions.MultivariateGaussian
    import breeze.stats.distributions.Rand.FixedSeed.randBasis

    def gaussianDiagSample(x : Vector[Double], sigma: Double): Vector[Double] = {
        val mvn = MultivariateGaussian(
            DenseVector(x.toArray),
            DenseMatrix.eye[Double](x.size) * sigma,
        )
        mvn.draw().toArray.toVector
    }
