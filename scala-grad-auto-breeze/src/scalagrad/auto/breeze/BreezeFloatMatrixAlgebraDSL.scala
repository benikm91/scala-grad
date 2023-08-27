package scalagrad.auto.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

import scala.reflect.{TypeTest, Typeable}

object BreezeFloatMatrixAlgebraDSL extends MatrixAlgebraDSL:
 
    given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar] = summon[Typeable[Float]]
    given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector] = summon[Typeable[DenseVector[Float]]]
    given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector] = summon[Typeable[Transpose[DenseVector[Float]]]]
    given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix] = summon[Typeable[DenseMatrix[Float]]]

    override type Scalar = Float
    override type ColumnVector = DenseVector[Float]
    override type RowVector = Transpose[DenseVector[Float]]
    override type Matrix = DenseMatrix[Float]
    override val innerAlgebra = BreezeFloatMatrixAlgebra