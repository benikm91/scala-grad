package scalagrad.api.reverse

import scalagrad.api.DeriverPlan
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.delta.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.derivative.OneOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.reverse.eval.Eval
import breeze.linalg.*
import scalagrad.api.reverse.DualDeltaDerivativeMatrixAlgebra

abstract class DeriverReversePlan[
    // TODO currently type bounds are needed to make the compiler happy, but they should not be needed
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
](
    primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
) extends DeriverPlan[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
    DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], 
    DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], 
    DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix],
    DualDeltaDerivativeMatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
](
    primaryMatrixAlgebra,
    DualDeltaDerivativeMatrixAlgebra(),
):

    type DScalar = DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
    type DColumnVector = DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DRowVector = DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DMatrix = DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

    type DualScalar = DualDeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
    type DualColumnVector = DualDeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DualRowVector = DualDeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DualMatrix = DualDeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

    type OneOpsT = OneOps[PScalar, PColumnVector, PRowVector, PMatrix]
    val oneOps: OneOpsT

    val eval = Eval[PScalar, PColumnVector, PRowVector, PMatrix](primaryMatrixAlgebra)

    import primaryMatrixAlgebra.*
    import derivativeMatrixAlgebra.*

    given scalar2Scalar: DeriverFromTo[DualScalar => DualScalar, PScalar => PScalar] with
        override def derive(f: DualScalar => DualScalar): PScalar => PScalar = x => 
            val delta = f(createDualScalar(x, DeltaScalar.Val(0))).dv
            val res = eval.evalScalar(oneOps.oneHotScalar, delta)
            res.scalars.get(0).getOrElse(zeroScalar)

    given columnVector2Scalar: DeriverFromTo[DualColumnVector => DualScalar, PColumnVector => PColumnVector] with
        override def derive(f: DualColumnVector => DualScalar): PColumnVector => PColumnVector = x => 
            val delta = f(createDualColumnVector(x, DeltaColumnVector.Val(0))).dv
            val res = eval.evalScalar(oneOps.oneHotScalar, delta)
            res.columnVectors.get(0).getOrElse(zeroColumnVector(x.length))

    given rowVector2Scalar: DeriverFromTo[DualRowVector => DualScalar, PRowVector => PRowVector] with
        override def derive(f: DualRowVector => DualScalar): PRowVector => PRowVector = x => 
            val delta = f(createDualRowVector(x, DeltaRowVector.Val(0))).dv
            val res = eval.evalScalar(oneOps.oneHotScalar, delta)
            res.rowVectors.get(0).getOrElse(zeroRowVector(x.length))

    given matrix2Scalar: DeriverFromTo[DualMatrix => DualScalar, PMatrix => PMatrix] with
        override def derive(f: DualMatrix => DualScalar): PMatrix => PMatrix = x => 
            val delta = f(createDualMatrix(x, DeltaMatrix.Val(0))).dv
            val res = eval.evalScalar(oneOps.oneHotScalar, delta)
            res.matrices.get(0).getOrElse(zeroMatrix(x.nRows, x.nCols))

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[T => DualScalar, DualTupleToPTuple[T] => DualTupleToPTuple[T]] with

        private val ids = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

        override def derive(f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T] = t => 
            def reversePlan(t: DualTupleToPTuple[T]): DualTupleToPTuple[T] = 
                val tWithIndex = t.zip(ids)
                val delta = f(
                    tWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                        case (x: PScalar, id: Int) => createDualScalar(x, DeltaScalar.Val(id))
                        case (x: PColumnVector, id: Int) => createDualColumnVector(x, DeltaColumnVector.Val(id))
                        case (x: PRowVector, id: Int) => createDualRowVector(x, DeltaRowVector.Val(id))
                        case (x: PMatrix, id: Int) => createDualMatrix(x, DeltaMatrix.Val(id))
                    }).asInstanceOf[T]
                ).dv.asInstanceOf[DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]]
                
                val res = eval.evalScalar(oneOps.oneHotScalar, delta)
                tWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (_: PScalar, id: Int) => res.scalars.get(id).getOrElse(zeroScalar)
                    case (cv: PColumnVector, id: Int) => res.columnVectors.get(id).getOrElse(zeroColumnVector(cv.length))
                    case (rv: PRowVector, id: Int) => res.rowVectors.get(id).getOrElse(zeroRowVector(rv.length))
                    case (m: PMatrix, id: Int) => res.matrices.get(id).getOrElse(zeroMatrix(m.nRows, m.nCols))
                }).asInstanceOf[DualTupleToPTuple[T]]
            resetIndex() // Reset index, so tracking of statement order is starting at 0 again.
            reversePlan(t)