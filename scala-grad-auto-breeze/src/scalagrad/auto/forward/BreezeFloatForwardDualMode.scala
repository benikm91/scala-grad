package scalagrad.auto.forward

import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.forward.ForwardDualMode
import scalagrad.auto.breeze.BreezeFloatMatrixAlgebra

object BreezeFloatForwardDualMode extends ForwardDualMode[
    Float, DenseVector[Float], Transpose[DenseVector[Float]], DenseMatrix[Float],
](BreezeFloatMatrixAlgebra)
