package scalagrad.api

import scala.runtime.Tuples
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.spire.trig.DualScalarIsTrig
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.dual.DualMatrixAlgebraDSL

abstract class Mode[
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

    import primaryMatrixAlgebra.given

    val algebra = DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ](
        primaryMatrixAlgebra,
        derivativeMatrixAlgebra,
    )

    given dualAlgebraGiven: DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ] = algebra

    val algebraT = new DualMatrixAlgebraDSL {
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

    given scalar2Scalar: DeriverFromTo[DualScalar => DualScalar, PScalar => PScalar]
    given columnVector2Scalar: DeriverFromTo[DualColumnVector => DualScalar, PColumnVector => PColumnVector]
    given rowVector2Scalar: DeriverFromTo[DualRowVector => DualScalar, PRowVector => PRowVector]
    given matrix2Scalar: DeriverFromTo[DualMatrix => DualScalar, PMatrix => PMatrix]

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[
        T => DualScalar,
        DualTupleToPTuple[T] => DualTupleToPTuple[T]
    ]

    given tuple2Tuple[T <: Tuple : DualTuple, RT <: Tuple : DualTuple]: DeriverFromTo[T => RT, 
        DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]]
    ]
