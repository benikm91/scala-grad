package scalagrad.auto.breeze

import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

object BreezeFloatMatrixAlgebraDSL extends MatrixAlgebraDSL:
    override type Scalar = Float
    override type ColumnVector = DenseVector[Float]
    override type RowVector = Transpose[DenseVector[Float]]
    override type Matrix = DenseMatrix[Float]
    override val innerAlgebra = BreezeFloatMatrixAlgebra