package scalagrad.auto.reverse

import breeze.linalg.{DenseVector, Transpose, DenseMatrix}
import scalagrad.api.reverse.ReverseDualMode
import scalagrad.auto.breeze.BreezeFloatMatrixAlgebra

object BreezeFloatReverseDualMode extends ReverseDualMode[
    Float, DenseVector[Float], Transpose[DenseVector[Float]], DenseMatrix[Float],
](BreezeFloatMatrixAlgebra)
