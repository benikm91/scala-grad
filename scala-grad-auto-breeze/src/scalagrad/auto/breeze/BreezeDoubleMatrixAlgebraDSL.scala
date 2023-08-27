package scalagrad.auto.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

import scala.reflect.Typeable

val stED = summon[Typeable[Double]]
val cvtED = summon[Typeable[DenseVector[Double]]]
val rvtED = summon[Typeable[Transpose[DenseVector[Double]]]]
val mtED = summon[Typeable[DenseMatrix[Double]]]

object BreezeDoubleMatrixAlgebraDSL extends MatrixAlgebraDSL:

    override given st: Typeable[Double] = stED
    override given cvt: Typeable[DenseVector[Double]] = cvtED
    override given rvt: Typeable[Transpose[DenseVector[Double]]] = rvtED
    override given mt: Typeable[DenseMatrix[Double]] = mtED

    override type Scalar = Double
    override type ColumnVector = DenseVector[Double]
    override type RowVector = Transpose[DenseVector[Double]]
    override type Matrix = DenseMatrix[Double]
    override val innerAlgebra = BreezeDoubleMatrixAlgebra