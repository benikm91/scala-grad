package scalagrad.api.dual

import scalagrad.api.matrixalgebra.TransposeOps
import scala.annotation.targetName
import scala.math.Fractional.Implicits._
import scalagrad.api.dual

trait MapDualOps[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar], 
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector], 
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector], 
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix],
](
    ptops: TransposeOps[PColumnVector, PRowVector, PMatrix],
):

    this: TransposeOps[DualColumnVector, DualRowVector, DualMatrix] =>

    def elementWiseOpM(m: DualMatrix, op: PScalar => PScalar, dOp: PScalar => PScalar): DualMatrix
    
    def columnWiseOpM(m: DualMatrix, op: PColumnVector => PColumnVector, dOp: PColumnVector => PColumnVector): DualMatrix

    def rowWiseOpM(m: DualMatrix, op: PRowVector => PRowVector, dOp: PRowVector => PRowVector): DualMatrix = 
        columnWiseOpM(
            m.t, 
            cv => 
                val rv = ptops.transposeColumVector(cv)
                ptops.transposeRowVector(op(rv)),
            cv => 
                val rv = ptops.transposeColumVector(cv)
                ptops.transposeRowVector(dOp(rv)),
        ).t

    def elementWiseOpCV(cv: DualColumnVector, op: PScalar => PScalar, dOp: PScalar => PScalar): DualColumnVector
    
    def elementWiseOpRV(rv: DualRowVector, op: PScalar => PScalar, dOp: PScalar => PScalar): DualRowVector = 
        elementWiseOpCV(rv.t, op, dOp).t

    extension (m: DualMatrix)
        @targetName("elementWiseOpM_Op_map")
        def mapDual(op: PScalar => PScalar, dOp: PScalar => PScalar): DualMatrix = m.mapDualElements(op, dOp)
        @targetName("elementWiseOpM_Op2")
        def mapDualElements(op: PScalar => PScalar, dOp: PScalar => PScalar): DualMatrix = elementWiseOpM(m, op, dOp)
        @targetName("columnWiseOpM_Op2")
        def mapDualColumns(op: PColumnVector => PColumnVector, dOp: PColumnVector => PColumnVector): DualMatrix = columnWiseOpM(m, op, dOp)
        @targetName("rowWiseOpM_Op2")
        def mapDualRows(op: PRowVector => PRowVector, dOp: PRowVector => PRowVector): DualMatrix = rowWiseOpM(m, op, dOp)

    extension (cv: DualColumnVector)
        @targetName("elementWiseOpCV_Op_map")
        def mapDual(op: PScalar => PScalar, dOp: PScalar => PScalar): DualColumnVector = cv.mapDualElements(op, dOp)
        @targetName("elementWiseOpCV_Op2")
        def mapDualElements(op: PScalar => PScalar, dOp: PScalar => PScalar): DualColumnVector = elementWiseOpCV(cv, op, dOp)

    extension (rv: DualRowVector)
        @targetName("elementWiseOpRV_Op_map")
        def mapDual(op: PScalar => PScalar, dOp: PScalar => PScalar): DualRowVector = rv.mapDualElements(op, dOp)
        @targetName("elementWiseOpRV_Op2")
        def mapDualElements(op: PScalar => PScalar, dOp: PScalar => PScalar): DualRowVector = elementWiseOpRV(rv, op, dOp)
