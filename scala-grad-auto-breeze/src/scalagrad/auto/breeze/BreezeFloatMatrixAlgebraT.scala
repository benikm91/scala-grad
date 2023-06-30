package scalagrad.auto.breeze

import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

object BreezeFloatMatrixAlgebraT extends MatrixAlgebraT:
    override type Scalar = Float
    override type ColumnVector = DenseVector[Float]
    override type RowVector = Transpose[DenseVector[Float]]
    override type Matrix = DenseMatrix[Float]
    override val innerAlgebra = BreezeFloatMatrixAlgebra