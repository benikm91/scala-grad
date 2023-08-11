package scalagrad.auto.breeze

import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

object BreezeDoubleMatrixAlgebraDSL extends MatrixAlgebraDSL:
    override type Scalar = Double
    override type ColumnVector = DenseVector[Double]
    override type RowVector = Transpose[DenseVector[Double]]
    override type Matrix = DenseMatrix[Double]
    override val innerAlgebra = BreezeDoubleMatrixAlgebra