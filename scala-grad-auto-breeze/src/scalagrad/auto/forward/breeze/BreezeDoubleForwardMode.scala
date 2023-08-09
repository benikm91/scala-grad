package scalagrad.auto.forward.breeze

import scalagrad.api.forward.ForwardMode
import breeze.linalg.*
import scalagrad.api.Deriver
import scalagrad.api.forward.dual.DualNumberMatrix
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.forward.dual.DualNumberRowVector
import scalagrad.api.forward.dual.DualNumberColumnVector
import scalagrad.api.matrixalgebra.CreateOps
import scala.reflect.ClassTag
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra

object BreezeDoubleForwardMode extends ForwardMode[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](
    BreezeDoubleMatrixAlgebra
)