package scalagrad.api.matrixalgebra

import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import scalagrad.api.fractional.MatrixAlgebraScalarIsFractional
import spire.algebra.{NRoot, Trig}
import spire.math.Numeric

import scala.reflect.TypeTest
import scala.annotation.targetName

trait MatrixAlgebraDSL:
  
    type Scalar
    type ColumnVector
    type RowVector
    type Matrix

    given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar]
    given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector]
    given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector]
    given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix]

    val innerAlgebra: MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix]

    export innerAlgebra.*
    export innerAlgebra.given


trait MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix](
        using 
        scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar],
        columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector],
        rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector],
        matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix],
    ) 
    extends LengthOps[ColumnVector, RowVector, Matrix]
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
    given frac: Fractional[Scalar] = MatrixAlgebraScalarIsFractional.given_Fractional_Scalar(using this)
    
    override def trace(m: Matrix): Scalar = 
        val n = m.nRows min m.nCols
        var res = lift(0)
        for i <- 0 until n do
            res = res + m.elementAt(i, i)
        res

    // TODO How to not define them here, but still be easy to use/lift with breeze
    def lift(i: Int): Scalar = lift(i.toDouble)
    def lift(d: Double): Scalar
    def lift(d: Float): Scalar
    def lift(cv: DenseVector[Double]): ColumnVector
    @targetName("liftCVFloat")
    def lift(cv: DenseVector[Float]): ColumnVector
    def lift(rv: Transpose[DenseVector[Double]]): RowVector
    @targetName("liftRVFloat")
    def lift(rv: Transpose[DenseVector[Float]]): RowVector
    def lift(m: DenseMatrix[Double]): Matrix
    @targetName("liftMFloat")
    def lift(m: DenseMatrix[Float]): Matrix
    def unlift(s: Scalar): Double
    def unlift(m: Matrix): DenseMatrix[Double]
    def unlift(cv: ColumnVector): DenseVector[Double]
    def unlift(rv: RowVector): Transpose[DenseVector[Double]]

    extension(s: Scalar)
        def toDouble = unlift(s)

    def asDSL: MatrixAlgebraDSL {
        type Scalar = ScalarT
        type ColumnVector = ColumnVectorT
        type RowVector = RowVectorT
        type Matrix = MatrixT
    } = 
        new MatrixAlgebraDSL {
            override given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar] = MatrixAlgebra.this.scalarTest
            override given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector] = MatrixAlgebra.this.columnVectorTest
            override given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector] = MatrixAlgebra.this.rowVectorTest
            override given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix] = MatrixAlgebra.this.matrixTest
            override type Scalar = MatrixAlgebra.this.ScalarT
            override type ColumnVector = MatrixAlgebra.this.ColumnVectorT
            override type RowVector = MatrixAlgebra.this.RowVectorT
            override type Matrix = MatrixAlgebra.this.MatrixT
            override val innerAlgebra = MatrixAlgebra.this
        }