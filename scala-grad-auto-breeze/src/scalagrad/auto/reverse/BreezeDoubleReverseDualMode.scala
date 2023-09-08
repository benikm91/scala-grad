package scalagrad.auto.reverse

import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.reverse.ReverseDualMode
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

object BreezeDoubleReverseDualMode extends ReverseDualMode[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](BreezeDoubleMatrixAlgebra)
