package scalagrad.auto.breeze

import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.{MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

import scala.reflect.{TypeTest, Typeable}

object BreezeFloatMatrixAlgebraDSL extends MatrixAlgebraDSL:
 
    given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar] = summon[Typeable[Float]]
    given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector] = 
        new TypeTest {
            override def unapply(x: Scalar | ColumnVector | RowVector | Matrix): Option[x.type & ColumnVector] = 
                x match {
                    case _: DenseVector[_] => Some(x.asInstanceOf[x.type & ColumnVector])
                    case _ => None
                }
            }
    given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector] = 
        new TypeTest {
            override def unapply(x: Scalar | ColumnVector | RowVector | Matrix): Option[x.type & RowVector] = 
                x match {
                    case _: Transpose[_] => Some(x.asInstanceOf[x.type & RowVector])
                    case _ => None
                }
            }
    given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix] = 
        new TypeTest {
            override def unapply(x: Scalar | ColumnVector | RowVector | Matrix): Option[x.type & Matrix] = 
                x match {
                    case _: DenseMatrix[_] => Some(x.asInstanceOf[x.type & Matrix])
                    case _ => None
                }
            }

    override type Scalar = Float
    override type ColumnVector = DenseVector[Float]
    override type RowVector = Transpose[DenseVector[Float]]
    override type Matrix = DenseMatrix[Float]
    override val innerAlgebra = BreezeFloatMatrixAlgebra