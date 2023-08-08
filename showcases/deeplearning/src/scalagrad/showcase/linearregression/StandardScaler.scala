package scalagrad.showcase.linearregression

object StandardScaler:

    def scaleMatrix(xs: Vector[Vector[Double]]): (Vector[Vector[Double]], Vector[Double], Vector[Double]) =
        val (xsScaledT, means, stds) = xs.transpose
            .map(scaleColumn)
            .unzip3
        (xsScaledT.transpose, means, stds)

    def inverseScaleMatrix(xs: Vector[Vector[Double]], means: Vector[Double], stds: Vector[Double]): Vector[Vector[Double]] =
        val xsScaledT = xs.transpose
            .zip(means)
            .zip(stds)
            .map { case ((xs, mean), std) => inverseScaleColumn(xs, mean, std) }
        xsScaledT.transpose

    def scaleColumn(xs: Vector[Double]): (Vector[Double], Double, Double) =
        val mean = xs.sum / xs.size
        val std = Math.sqrt(xs.map(x => Math.pow(x - mean, 2)).sum / xs.size)
        (xs.map(x => (x - mean) / std), mean, std)

    def inverseScaleColumn(xs: Vector[Double], mean: Double, std: Double): Vector[Double] =
        xs.map(x => x * std + mean)