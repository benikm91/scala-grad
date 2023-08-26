package scalagrad.api

import scala.runtime.Tuples
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.spire.trig.DualScalarIsTrig
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scala.reflect.Typeable
import scala.annotation.targetName
import scalagrad.api.forward.ForwardDualMode
import scalagrad.api.reverse.ReverseDualMode


trait DualMode[
    PScalar, PColumnVector, PRowVector, PMatrix,
]:

    val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix]
    
    type DScalar
    type DColumnVector
    type DRowVector
    type DMatrix

    type DualScalar <: dual.DualScalar[PScalar, DScalar]
    type DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector]
    type DualRowVector <: dual.DualRowVector[PRowVector, DRowVector]
    type DualMatrix <: dual.DualMatrix[PMatrix, DMatrix]

    val derivativeMatrixAlgebra: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ]

    import primaryMatrixAlgebra.given

    lazy val algebra = DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ](
        primaryMatrixAlgebra,
        derivativeMatrixAlgebra,
    )

    val stE = summon[Typeable[DualScalar]]
    val cvtE = summon[Typeable[DualColumnVector]]
    val rvtE = summon[Typeable[DualRowVector]]
    val mtE = summon[Typeable[DualMatrix]]

    lazy val algebraDSL = new DualMatrixAlgebraDSL {

        override given st: Typeable[Scalar] = stE
        override given cvt: Typeable[ColumnVector] = cvtE
        override given rvt: Typeable[RowVector] = rvtE
        override given mt: Typeable[Matrix] = mtE

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

    def deriveDualTuple2Scalar[T <: Tuple : DualTuple](f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T]
           
    def deriveDualTuple2ColumnVector[T <: Tuple : DualTuple](f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]
    
    def deriveDualTuple2RowVector[T <: Tuple : DualTuple](f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]
    
    def deriveDualTuple2Matrix[T <: Tuple : DualTuple](f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]
        
    def deriveScalar2DualTuple[RT <: Tuple : DualTuple](f: DualScalar => RT): PScalar => DualTupleToPTuple[RT]
    
    def deriveColumnVector2DualTuple[RT <: Tuple : DualTuple](f: DualColumnVector => RT): PColumnVector => DualTupleToPTuple[RT]
    
    def deriveRowVector2DualTuple[RT <: Tuple : DualTuple](f: DualRowVector => RT): PRowVector => DualTupleToPTuple[RT]
    
    def deriveMatrix2DualTuple[RT <: Tuple : DualTuple](f: DualMatrix => RT): PMatrix => DualTupleToPTuple[RT]
    
    def deriveDualTuple2DualTuple[T <: Tuple : DualTuple, RT <: Tuple : DualTuple](f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]]

    type DualTuple[T <: Tuple] = T match
        case DualScalar *: t => DualTuple[t]
        case DualColumnVector *: t => DualTuple[t]
        case DualRowVector *: t => DualTuple[t]
        case DualMatrix *: t => DualTuple[t]
        case EmptyTuple => DummyImplicit

    type DualTupleToPTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => DualToP[head] *: DualTupleToPTuple[tail]
        case EmptyTuple     => EmptyTuple

    type DualToP[T] = T match
        case DualScalar       => PScalar
        case DualColumnVector => PColumnVector
        case DualRowVector    => PRowVector
        case DualMatrix       => PMatrix

    type UpPByColumnVectorTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByColumnVector[head] *: UpPByColumnVectorTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpPByColumnVector[T] = T match
        case PScalar       => PColumnVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByRowVectorTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByRowVector[head] *: UpPByRowVectorTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpPByRowVector[T] = T match
        case PScalar       => PRowVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByMatrixTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByMatrix[head] *: UpPByMatrixTuple[tail]
        case EmptyTuple     => EmptyTuple

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

