package scalagrad.auto.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

import scala.reflect.{Typeable, TypeTest}

object BreezeDoubleMatrixAlgebraDSL extends MatrixAlgebraDSL:

    given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar] = summon[Typeable[Double]]
    given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector] = summon[Typeable[DenseVector[Double]]]
    given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector] = summon[Typeable[Transpose[DenseVector[Double]]]]
    given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix] = summon[Typeable[DenseMatrix[Double]]]

    override type Scalar = Double
    override type ColumnVector = DenseVector[Double]
    override type RowVector = Transpose[DenseVector[Double]]
    override type Matrix = DenseMatrix[Double]
    override val innerAlgebra = BreezeDoubleMatrixAlgebra