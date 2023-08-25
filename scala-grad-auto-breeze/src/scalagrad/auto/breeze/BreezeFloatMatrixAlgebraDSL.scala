package scalagrad.auto.breeze

import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.*
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra
import scala.reflect.Typeable

val stEF = summon[Typeable[Float]]
val cvtEF = summon[Typeable[DenseVector[Float]]]
val rvtEF = summon[Typeable[Transpose[DenseVector[Float]]]]
val mtEF = summon[Typeable[DenseMatrix[Float]]]

object BreezeFloatMatrixAlgebraDSL extends MatrixAlgebraDSL:
    

    override given st: Typeable[Float] = stEF

    override given cvt: Typeable[DenseVector[Float]] = cvtEF

    override given rvt: Typeable[Transpose[DenseVector[Float]]] = rvtEF

    override given mt: Typeable[DenseMatrix[Float]] = mtEF

    override type Scalar = Float
    override type ColumnVector = DenseVector[Float]
    override type RowVector = Transpose[DenseVector[Float]]
    override type Matrix = DenseMatrix[Float]
    override val innerAlgebra = BreezeFloatMatrixAlgebra