package scalagrad.api

import scalagrad.api.dual.{DualMatrixAlgebra, DualMatrixAlgebraDSL}
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra

import scala.reflect.TypeTest
import scala.annotation.targetName


trait DualMode[
    PScalarT, PColumnVectorT, PRowVectorT, PMatrixT,
    DScalarT, DColumnVectorT, DRowVectorT, DMatrixT, 
    DualScalarT <: dual.DualScalar[PScalarT, DScalarT],
    DualColumnVectorT <: dual.DualColumnVector[PColumnVectorT, DColumnVectorT],
    DualRowVectorT <: dual.DualRowVector[PRowVectorT, DRowVectorT],
    DualMatrixT <: dual.DualMatrix[PMatrixT, DMatrixT],
](
    val algebra: DualMatrixAlgebra[
        PScalarT, PColumnVectorT, PRowVectorT, PMatrixT, 
        DScalarT, DColumnVectorT, DRowVectorT, DMatrixT, 
        DualScalarT, DualColumnVectorT, DualRowVectorT, DualMatrixT
    ]
):

    type PScalar = PScalarT
    type PColumnVector = PColumnVectorT
    type PRowVector = PRowVectorT
    type PMatrix = PMatrixT

    type DScalar = DScalarT
    type DColumnVector = DColumnVectorT
    type DRowVector = DRowVectorT
    type DMatrix = DMatrixT

    type DualScalar = DualScalarT
    type DualColumnVector = DualColumnVectorT
    type DualRowVector = DualRowVectorT
    type DualMatrix = DualMatrixT
    
    val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix] = algebra.primaryMatrixAlgebra

    val derivativeMatrixAlgebra: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ] = algebra.derivativeMatrixAlgebra

    import primaryMatrixAlgebra.given

    given dualScalarTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualScalar]

    given dualColumnVectorTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualColumnVector]

    given dualRowVectorTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualRowVector]
    
    given dualMatrixTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualMatrix]

    given algebraGiven: MatrixAlgebra[DualScalar, DualColumnVector, DualRowVector, DualMatrix] = algebra

    val algebraDSL = new DualMatrixAlgebraDSL {

        given scalarTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Scalar] = dualScalarTest
        given columnVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, ColumnVector] = dualColumnVectorTest
        given rowVectorTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, RowVector] = dualRowVectorTest
        given matrixTest: TypeTest[Scalar | ColumnVector | RowVector | Matrix, Matrix] = dualMatrixTest

        override type PrimaryScalar = PScalar
        override type PrimaryColumnVector = PColumnVector
        override type PrimaryRowVector = PRowVector
        override type PrimaryMatrix = PMatrix
        override type DerivativeScalar = DScalar
        override type DerivativeColumnVector = DColumnVector
        override type DerivativeRowVector = DRowVector
        override type DerivativeMatrix = DMatrix
        override type Scalar = DualScalar
        override type ColumnVector = DualColumnVector
        override type RowVector = DualRowVector
        override type Matrix = DualMatrix
        override val innerAlgebra = algebra
    }

    @targetName("deriveDualTuple2Scalar")
    def derive[T <: Tuple : DualTuple](f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T]

    @targetName("deriveDualTuple2ColumnVector")       
    def derive[T <: Tuple : DualTuple](f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]
    
    @targetName("deriveDualTuple2RowVector")
    def derive[T <: Tuple : DualTuple](f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]
    
    @targetName("deriveDualTuple2Matrix")
    def derive[T <: Tuple : DualTuple](f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]

    @targetName("deriveScalar2DualTuple")    
    def derive[RT <: Tuple : DualTuple](f: DualScalar => RT): PScalar => DualTupleToPTuple[RT]
    
    @targetName("deriveColumnVector2DualTuple")
    def derive[RT <: Tuple : DualTuple](f: DualColumnVector => RT): PColumnVector => DualTupleToPTuple[RT]
    
    @targetName("deriveRowVector2DualTuple")
    def derive[RT <: Tuple : DualTuple](f: DualRowVector => RT): PRowVector => DualTupleToPTuple[RT]
    
    @targetName("deriveMatrix2DualTuple")
    def derive[RT <: Tuple : DualTuple](f: DualMatrix => RT): PMatrix => DualTupleToPTuple[RT]
    
    @targetName("deriveDualTuple2DualTuple")
    def derive[T <: Tuple : DualTuple, RT <: Tuple : DualTuple](f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]]

    type DualTuple[T <: Tuple] = T match
        case DualScalar *: t => DualTuple[t]
        case DualColumnVector *: t => DualTuple[t]
        case DualRowVector *: t => DualTuple[t]
        case DualMatrix *: t => DualTuple[t]
        case EmptyTuple => DummyImplicit

    type DualTupleToPTuple[T <: Tuple] = Tuple.Map[T, DualToP]

    type DualToP[T] = T match
        case DualScalar       => PScalar
        case DualColumnVector => PColumnVector
        case DualRowVector    => PRowVector
        case DualMatrix       => PMatrix

    type UpPByColumnVectorTuple[T <: Tuple] = Tuple.Map[T, UpPByColumnVector]
    
    type UpPByColumnVector[T] = T match
        case PScalar       => PColumnVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByRowVectorTuple[T <: Tuple] = Tuple.Map[T, UpPByRowVector]

    type UpPByRowVector[T] = T match
        case PScalar       => PRowVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByMatrixTuple[T <: Tuple] = Tuple.Map[T, UpPByMatrix]

    type UpPByMatrix[T] = T match
        case PScalar       => PMatrix
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type CopyAndUpP = [T <: Tuple] =>> [X] =>> X match
        case DualScalar => T
        case DualColumnVector => UpPByColumnVectorTuple[T]
        case DualRowVector => UpPByRowVectorTuple[T]
        case DualMatrix => UpPByMatrixTuple[T]

    type CartesianProductAndUpP[T1 <: Tuple, T2 <: Tuple] = Tuple.Map[T1, CopyAndUpP[T2]]

