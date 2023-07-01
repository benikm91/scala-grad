package scalagrad.api

import scala.runtime.Tuples
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.spire.trig.DualScalarIsTrig
import scalagrad.api.matrixalgebra.MatrixAlgebraT
import scalagrad.api.dual.DualMatrixAlgebraT

abstract class DeriverPlan[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar], 
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector],
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector],
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
    DerivativeMatrixAlgebraT <: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ]
](
    val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
    val derivativeMatrixAlgebra: DerivativeMatrixAlgebraT
):

    val algebra = DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ](
        primaryMatrixAlgebra,
        derivativeMatrixAlgebra,
    )

    given algebraGiven: MatrixAlgebra[
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ] = algebra

    given primaryMatrixAlgebraGiven: MatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix
    ] = primaryMatrixAlgebra

    given derivativeMatrixAlgebraGiven: DerivativeMatrixAlgebraT = derivativeMatrixAlgebra

    val algebraT = new DualMatrixAlgebraT {
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

    type UpPTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpP[head] *: UpPTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpP[T] = T match
        case PScalar       => PColumnVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => DualMatrix

    type PToDual[T] = T match
        case PScalar       => DualScalar      
        case PColumnVector => DualColumnVector
        case PRowVector    => DualRowVector   
        case PMatrix       => DualMatrix      

    type CopyAndUpP = [T <: Tuple] =>> [X] =>> X match
        case DualScalar => T
        case DualColumnVector => UpPTuple[T]
        case DualRowVector => UpPTuple[T]
        case DualMatrix => T

    type CartesianProductAndUpP[T1 <: Tuple, T2 <: Tuple] = Tuple.Map[T1, CopyAndUpP[T2]]

    given tuple2Tuple[T <: Tuple : DualTuple, RT <: Tuple : DualTuple]: DeriverFromTo[T => RT, 
        DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]]
    ]

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[
        T => DualScalar,
        DualTupleToPTuple[T] => DualTupleToPTuple[T]
    ]
