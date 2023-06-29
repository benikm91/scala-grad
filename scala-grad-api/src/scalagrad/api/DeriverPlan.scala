package scalagrad.api

import scala.runtime.Tuples
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra

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

    type PToDual[T] = T match
        case PScalar       => DualScalar      
        case PColumnVector => DualColumnVector
        case PRowVector    => DualRowVector   
        case PMatrix       => DualMatrix      

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[
        T => DualScalar,
        DualTupleToPTuple[T] => DualTupleToPTuple[T]
    ]
