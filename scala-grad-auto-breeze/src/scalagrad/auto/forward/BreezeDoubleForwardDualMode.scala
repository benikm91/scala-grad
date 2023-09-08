package scalagrad.auto.forward

import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.forward.ForwardDualMode
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

object BreezeDoubleForwardDualMode extends ForwardDualMode[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](BreezeDoubleMatrixAlgebra)
