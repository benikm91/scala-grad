package scalagrad.showcase.linearregression

object StandardScaler:

    /**
     * @param xs The matrix to scale.
     * @return (xsScaled, means, stds) where xsScaled is the scaled matrix, means is the mean of each column and stds is the standard deviation of each column.
     */
    def scaleMatrix(xs: Vector[Vector[Double]]): (Vector[Vector[Double]], Vector[Double], Vector[Double]) =
        val (xsScaledT, means, stds) = xs.transpose
            .map(scaleColumn)
            .unzip3
        (xsScaledT.transpose, means, stds)

    /**
      * Undo scaleMatrix
      *
      * @param xs The scaled matrix.
      * @param means The means of each column by which the matrix was scaled.
      * @param stds The standard deviations of each column by which the matrix was scaled.
      * @return The unscaled matrix.
      */
    def inverseScaleMatrix(xs: Vector[Vector[Double]], means: Vector[Double], stds: Vector[Double]): Vector[Vector[Double]] =
        val xsScaledT = xs.transpose
            .zip(means)
            .zip(stds)
            .map { case ((xs, mean), std) => inverseScaleColumn(xs, mean, std) }
        xsScaledT.transpose

    /**
      * @param xs The vector to scale.
      * @return (xsScaled, mean, std) where xsScaled is the scaled vector, mean is the mean of the vector and std is the standard deviation of the vector.
      */
    def scaleColumn(xs: Vector[Double]): (Vector[Double], Double, Double) =
        val mean = xs.sum / xs.size
        val std = Math.sqrt(xs.map(x => Math.pow(x - mean, 2)).sum / xs.size)
        (xs.map(x => (x - mean) / std), mean, std)

    /**
     * Undo scaleColumn
     *
     * @param xs The scaled vector.
     * @param mean The mean by which the vector was scaled.
     * @param std The standard deviation by which the vector was scaled.
     * @return The unscaled vector.
     */
    def inverseScaleColumn(xs: Vector[Double], mean: Double, std: Double): Vector[Double] =
        xs.map(x => x * std + mean)