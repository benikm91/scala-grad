package scalagrad.api.matrixalgebra

import spire.math.Numeric
import spire.algebra.Trig
import spire.algebra.NRoot
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.reflect.Typeable

trait MatrixAlgebraDSL:
  
    type Scalar
    type ColumnVector
    type RowVector
    type Matrix

    given st: Typeable[Scalar]
    given cvt: Typeable[ColumnVector]
    given rvt: Typeable[RowVector]
    given mt: Typeable[Matrix]

    val innerAlgebra: MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix]
    export innerAlgebra.*
    export innerAlgebra.given


trait MatrixAlgebra[Scalar : Typeable, ColumnVector : Typeable, RowVector : Typeable, Matrix : Typeable] 
    extends LengthOps[ColumnVector, RowVector, Matrix]
    with LiftOps[Scalar]
    with MatrixOps[Matrix, Scalar]
    with TransposeOps[ColumnVector, RowVector, Matrix]
    with NegateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ScalarInvertOps[Scalar]
    with One[Scalar]
    with OneOps[Scalar, ColumnVector, RowVector, Matrix]
    with ZeroOps[Scalar, ColumnVector, RowVector, Matrix]
    with BasicOps[Scalar, ColumnVector, RowVector, Matrix]
    with SumOps[Scalar, ColumnVector, RowVector, Matrix]
    with FoldLeftOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessSetOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessSeqOps[Scalar, ColumnVector, RowVector, Matrix]
    with CreateOps[Scalar, ColumnVector, RowVector, Matrix]
    with MapOps[Scalar, ColumnVector, RowVector, Matrix]
:

    type ScalarT = Scalar
    type ColumnVectorT = ColumnVector
    type RowVectorT = RowVector
    type MatrixT = Matrix

    given num: Numeric[Scalar]
    given trig: Trig[Scalar]

    override def trace(m: Matrix): Scalar = 
        val n = m.nRows min m.nCols
        var res = liftToScalar(0)
        for i <- 0 until n do
            res = res + m.elementAt(i, i)
        res

    // TODO How to not define them here, but still be easy to use/lift with breeze
    def liftBreezeMatrix(m: DenseMatrix[Double]): Matrix
    def liftBreezeVector(cv: DenseVector[Double]): ColumnVector

    def asDSL: MatrixAlgebraDSL {
        type Scalar = ScalarT
        type ColumnVector = ColumnVectorT
        type RowVector = RowVectorT
        type Matrix = MatrixT
    } = 
        val self = this
        val stE = summon[Typeable[Scalar]]
        val cvtE = summon[Typeable[ColumnVector]]
        val rvtE = summon[Typeable[RowVector]]
        val mtE = summon[Typeable[Matrix]]
        new MatrixAlgebraDSL {
            override given st: Typeable[Scalar] = stE
            override given cvt: Typeable[ColumnVector] = cvtE
            override given rvt: Typeable[RowVector] = rvtE
            override given mt: Typeable[Matrix] = mtE
            override type Scalar = self.ScalarT
            override type ColumnVector = self.ColumnVectorT
            override type RowVector = self.RowVectorT
            override type Matrix = self.MatrixT
            override val innerAlgebra = self
        }