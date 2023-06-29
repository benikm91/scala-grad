package scalagrad.auto.forward.breeze

import scalagrad.api.forward.DeriverForwardPlan
import breeze.linalg.*
import scalagrad.api.Deriver
import scalagrad.api.forward.dual.DualNumberMatrix
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.forward.dual.DualNumberRowVector
import scalagrad.api.forward.dual.DualNumberColumnVector
import scalagrad.api.matrixalgebra.CreateOps
import scala.reflect.ClassTag
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeMatrixAlgebra
import scalagrad.api.dual.DualMatrixAlgebra

object DeriverBreezeForwardPlan extends DeriverForwardPlan[
    Double, DenseVector[Double], Transpose[DenseVector[Double]], DenseMatrix[Double],
](
    BreezeMatrixAlgebra
):

    type PScalar = Double
    type PColumnVector = DenseVector[Double]
    type PRowVector = Transpose[DenseVector[Double]]
    type PMatrix = DenseMatrix[Double]
