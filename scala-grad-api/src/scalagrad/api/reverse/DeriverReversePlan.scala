package scalagrad.api.reverse

import scalagrad.api.DeriverPlan
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.delta.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.OneOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.reverse.eval.Eval
import scalagrad.api.reverse.DualDeltaDerivativeMatrixAlgebra
import scala.reflect.Typeable

abstract class DeriverReversePlan[
    PScalar : Typeable, PColumnVector : Typeable, PRowVector : Typeable, PMatrix : Typeable,
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

    val eval = Eval[PScalar, PColumnVector, PRowVector, PMatrix](primaryMatrixAlgebra)

    import primaryMatrixAlgebra.*
    import derivativeMatrixAlgebra.*

    // TODO can we move this to DeriverPlan?
    given scalar2Scalar: DeriverFromTo[DualScalar => DualScalar, PScalar => PScalar] with
        override def derive(f: DualScalar => DualScalar): PScalar => PScalar = x => 
            tuple2Scalar[Tuple1[DualScalar]].derive((t: Tuple1[DualScalar]) => f(t.head))(Tuple1(x)).head

    given columnVector2Scalar: DeriverFromTo[DualColumnVector => DualScalar, PColumnVector => PColumnVector] with
        override def derive(f: DualColumnVector => DualScalar): PColumnVector => PColumnVector = x => 
            tuple2Scalar[Tuple1[DualColumnVector]].derive((t: Tuple1[DualColumnVector]) => f(t.head))(Tuple1(x)).head

    given rowVector2Scalar: DeriverFromTo[DualRowVector => DualScalar, PRowVector => PRowVector] with
        override def derive(f: DualRowVector => DualScalar): PRowVector => PRowVector = x => 
            tuple2Scalar[Tuple1[DualRowVector]].derive((t: Tuple1[DualRowVector]) => f(t.head))(Tuple1(x)).head

    given matrix2Scalar: DeriverFromTo[DualMatrix => DualScalar, PMatrix => PMatrix] with
        override def derive(f: DualMatrix => DualScalar): PMatrix => PMatrix = x => 
            tuple2Scalar[Tuple1[DualMatrix]].derive((t: Tuple1[DualMatrix]) => f(t.head))(Tuple1(x)).head
    
    given scalar2ColumnVector: DeriverFromTo[DualScalar => DualColumnVector, PScalar => PColumnVector] with
        override def derive(f: DualScalar => DualColumnVector): PScalar => PColumnVector = x => 
            tuple2ColumnVector[Tuple1[DualScalar]].derive((t: Tuple1[DualScalar]) => f(t.head))(Tuple1(x)).head

    given columnVector2ColumnVector: DeriverFromTo[DualColumnVector => DualColumnVector, PColumnVector => UpPByColumnVector[PColumnVector]] with
        override def derive(f: DualColumnVector => DualColumnVector): PColumnVector => UpPByColumnVector[PColumnVector] = x => 
            tuple2ColumnVector[Tuple1[DualColumnVector]].derive((t: Tuple1[DualColumnVector]) => f(t.head))(Tuple1(x)).head

    given rowVector2ColumnVector: DeriverFromTo[DualRowVector => DualColumnVector, PRowVector => UpPByColumnVector[PRowVector]] with
        override def derive(f: DualRowVector => DualColumnVector): PRowVector => UpPByColumnVector[PRowVector] = x => 
            tuple2ColumnVector[Tuple1[DualRowVector]].derive((t: Tuple1[DualRowVector]) => f(t.head))(Tuple1(x)).head

    given matrix2ColumnVector: DeriverFromTo[DualMatrix => DualColumnVector, PMatrix => UpPByColumnVector[PMatrix]] with
        override def derive(f: DualMatrix => DualColumnVector): PMatrix => UpPByColumnVector[PMatrix] = x => 
            tuple2ColumnVector[Tuple1[DualMatrix]].derive((t: Tuple1[DualMatrix]) => f(t.head))(Tuple1(x)).head

    given scalar2RowVector: DeriverFromTo[DualScalar => DualRowVector, PScalar => PRowVector] with
        override def derive(f: DualScalar => DualRowVector): PScalar => PRowVector = x => 
            tuple2RowVector[Tuple1[DualScalar]].derive((t: Tuple1[DualScalar]) => f(t.head))(Tuple1(x)).head

    given columnVector2RowVector: DeriverFromTo[DualColumnVector => DualRowVector, PColumnVector => UpPByRowVector[PColumnVector]] with
        override def derive(f: DualColumnVector => DualRowVector): PColumnVector => UpPByRowVector[PColumnVector] = x => 
            tuple2RowVector[Tuple1[DualColumnVector]].derive((t: Tuple1[DualColumnVector]) => f(t.head))(Tuple1(x)).head

    given rowVector2RowVector: DeriverFromTo[DualRowVector => DualRowVector, PRowVector => UpPByRowVector[PRowVector]] with
        override def derive(f: DualRowVector => DualRowVector): PRowVector => UpPByRowVector[PRowVector] = x => 
            tuple2RowVector[Tuple1[DualRowVector]].derive((t: Tuple1[DualRowVector]) => f(t.head))(Tuple1(x)).head

    given matrix2RowVector: DeriverFromTo[DualMatrix => DualRowVector, PMatrix => UpPByRowVector[PMatrix]] with
        override def derive(f: DualMatrix => DualRowVector): PMatrix => UpPByRowVector[PMatrix] = x => 
            tuple2RowVector[Tuple1[DualMatrix]].derive((t: Tuple1[DualMatrix]) => f(t.head))(Tuple1(x)).head

    given scalar2Matrix: DeriverFromTo[DualScalar => DualMatrix, PScalar => PMatrix] with
        override def derive(f: DualScalar => DualMatrix): PScalar => PMatrix = x => 
            tuple2Matrix[Tuple1[DualScalar]].derive((t: Tuple1[DualScalar]) => f(t.head))(Tuple1(x)).head

    given columnVector2Matrix: DeriverFromTo[DualColumnVector => DualMatrix, PColumnVector => UpPByMatrix[PColumnVector]] with
        override def derive(f: DualColumnVector => DualMatrix): PColumnVector => UpPByMatrix[PColumnVector] = x => 
            tuple2Matrix[Tuple1[DualColumnVector]].derive((t: Tuple1[DualColumnVector]) => f(t.head))(Tuple1(x)).head

    given rowVector2Matrix: DeriverFromTo[DualRowVector => DualMatrix, PRowVector => UpPByMatrix[PRowVector]] with
        override def derive(f: DualRowVector => DualMatrix): PRowVector => UpPByMatrix[PRowVector] = x => 
            tuple2Matrix[Tuple1[DualRowVector]].derive((t: Tuple1[DualRowVector]) => f(t.head))(Tuple1(x)).head

    given matrix2Matrix: DeriverFromTo[DualMatrix => DualMatrix, PMatrix => UpPByMatrix[PMatrix]] with
        override def derive(f: DualMatrix => DualMatrix): PMatrix => UpPByMatrix[PMatrix] = x => 
            tuple2Matrix[Tuple1[DualMatrix]].derive((t: Tuple1[DualMatrix]) => f(t.head))(Tuple1(x)).head

    given tuple2Scalar[T <: Tuple : DualTuple]: DeriverFromTo[T => DualScalar, DualTupleToPTuple[T] => DualTupleToPTuple[T]] with

        override def derive(f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T] = t =>
            tuple2Tuple[T, Tuple1[DualScalar]].derive(t => Tuple1(f(t)))(t).map[[X] =>> Any](
                [T] => (e: T) =>
                    e.asInstanceOf[Tuple1[Any]].head
            ).asInstanceOf[DualTupleToPTuple[T]]
           
    given tuple2ColumnVector[T <: Tuple : DualTuple]: DeriverFromTo[T => DualColumnVector, DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]] with

        override def derive(f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector] = t =>
            tuple2Tuple[T, Tuple1[DualColumnVector]].derive(t => Tuple1(f(t)))(t).map[[X] =>> Any](
                [T] => (e: T) =>
                    e.asInstanceOf[Tuple1[Any]].head
            ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]]
    
    given tuple2RowVector[T <: Tuple : DualTuple]: DeriverFromTo[T => DualRowVector, DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]] with

        override def derive(f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector] = t =>
            tuple2Tuple[T, Tuple1[DualRowVector]].derive(t => Tuple1(f(t)))(t).map[[X] =>> Any](
                [T] => (e: T) =>
                    e.asInstanceOf[Tuple1[Any]].head
            ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]]
    
    given tuple2Matrix[T <: Tuple : DualTuple]: DeriverFromTo[T => DualMatrix, DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]] with

        override def derive(f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix] = t =>
            tuple2Tuple[T, Tuple1[DualMatrix]].derive(t => Tuple1(f(t)))(t).map[[X] =>> Any](
                [T] => (e: T) =>
                    e.asInstanceOf[Tuple1[Any]].head
            ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]]
        
    given scalar2Tuple[RT <: Tuple : DualTuple]: DeriverFromTo[DualScalar => RT, PScalar => DualTupleToPTuple[RT]] with
        override def derive(f: DualScalar => RT): PScalar => DualTupleToPTuple[RT] = t =>
            tuple2Tuple[Tuple1[DualScalar], RT].derive(t => f(t.head))(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    given columnVector2Tuple[RT <: Tuple : DualTuple]: DeriverFromTo[DualColumnVector => RT, PColumnVector => DualTupleToPTuple[RT]] with
        override def derive(f: DualColumnVector => RT): PColumnVector => DualTupleToPTuple[RT] = t =>
            tuple2Tuple[Tuple1[DualColumnVector], RT].derive(t => f(t.head))(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    given rowVector2Tuple[RT <: Tuple : DualTuple]: DeriverFromTo[DualRowVector => RT, PRowVector => DualTupleToPTuple[RT]] with
        override def derive(f: DualRowVector => RT): PRowVector => DualTupleToPTuple[RT] = t =>
            tuple2Tuple[Tuple1[DualRowVector], RT].derive(t => f(t.head))(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    given matrix2Tuple[RT <: Tuple : DualTuple]: DeriverFromTo[DualMatrix => RT, PMatrix => DualTupleToPTuple[RT]] with
        override def derive(f: DualMatrix => RT): PMatrix => DualTupleToPTuple[RT] = t =>
            tuple2Tuple[Tuple1[DualMatrix], RT].derive(t => f(t.head))(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    given tuple2Tuple[T <: Tuple : DualTuple, RT <: Tuple : DualTuple]: DeriverFromTo[T => RT, 
        DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]]
    ] with

        private val ids = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

        override def derive(f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = t =>
            def reversePlan(t: DualTupleToPTuple[T]): CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = 
                val tWithIndex = t.zip(ids)
                val output = f(
                    tWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                        case (x: PScalar, id: Int) => createDualScalar(x, DeltaScalar.Val(0, DeltaScalar.Input(id)))
                        case (x: PColumnVector, id: Int) => createDualColumnVector(x, DeltaColumnVector.Val(0, DeltaColumnVector.Input(id)))
                        case (x: PRowVector, id: Int) => createDualRowVector(x, DeltaRowVector.Val(0, DeltaRowVector.Input(id)))
                        case (x: PMatrix, id: Int) => createDualMatrix(x, DeltaMatrix.Val(0, DeltaMatrix.Input(id)))
                    }).asInstanceOf[T]
                )
                def extractResults(res: eval.Results): DualTupleToPTuple[RT] =
                    tWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                        case (_: PScalar, id: Int) => res.scalars.get(id).getOrElse(zeroScalar)
                        case (cv: PColumnVector, id: Int) => res.columnVectors.get(id).getOrElse(zeroColumnVector(cv.length))
                        case (rv: PRowVector, id: Int) => res.rowVectors.get(id).getOrElse(zeroRowVector(rv.length))
                        case (m: PMatrix, id: Int) => res.matrices.get(id).getOrElse(zeroMatrix(m.nRows, m.nCols))
                    }).asInstanceOf[DualTupleToPTuple[RT]]
                // map over outputs
                val res = output.map[[X] =>> Any]([U] => (t: U) => t match {
                    case x: (DualScalar @unchecked) => 
                        // run reverse plan for scalar
                        val res = eval.evalScalar(one, x.delta)
                        extractResults(res)
                    case x: (DualColumnVector @unchecked) =>
                        // run reverse plan for column vector
                        val inputsByOutputElements = for (i <- 0 until x.length) yield { 
                            val res = eval.evalColumnVector(oneHotColumnVector(x.length, i), x.delta)
                            extractResults(res)
                        }
                        // map over input
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (_: PScalar, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PScalar])
                                createColumnVectorFromElements(inputByOutputsElements)
                            case (_: PColumnVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PColumnVector])
                                stackColumns(inputByOutputsElements)
                            case (_: PRowVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PRowVector])
                                stackRows(inputByOutputsElements).t
                            case (m: PMatrix, index: Int) =>
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PMatrix])
                                val nInputElements = inputByOutputsElements.head.nRows * inputByOutputsElements.head.nCols
                                createMatrixFromElements(nInputElements, inputByOutputsElements.length, inputByOutputsElements.flatMap(_.elements))
                        })
                    case x: (DualRowVector @unchecked) => 
                        // run reverse plan for row vector
                        val inputsByOutputElements = for (i <- 0 until x.length) yield { 
                            val res = eval.evalRowVector(oneHotRowVector(x.length, i), x.delta)
                            extractResults(res)
                        }
                        // map over inputs
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (x: PScalar, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PScalar])
                                createRowVectorFromElements(inputByOutputsElements)
                            case (x: PColumnVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PColumnVector])
                                stackColumns(inputByOutputsElements)
                            case (x: PRowVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PRowVector])
                                stackRows(inputByOutputsElements).t
                            case (x: PMatrix, index: Int) =>
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PMatrix])
                                val nInputElements = inputByOutputsElements.head.nRows * inputByOutputsElements.head.nCols
                                val cols = inputByOutputsElements.map(_.t.elements).map(x => createColumnVectorFromElements(x))
                                stackColumns(cols)
                        })
                    case x: (DualMatrix @unchecked) => 
                        // run reverse plan for matrix
                        val inputsByOutputElements = for (i <- 0 until x.v.nRows * x.v.nCols) yield {
                            val res = eval.evalMatrix(oneHotMatrix(x.v.nRows, x.v.nCols, i), x.delta)
                            extractResults(res)
                        }
                        // map over inputs
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (_: PScalar, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PScalar])
                                createMatrixFromElements(x.v.nRows, x.v.nCols, inputByOutputsElements)
                            case (_: PColumnVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PColumnVector])
                                stackColumns(inputByOutputsElements)
                            case (_: PRowVector, index: Int) => 
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PRowVector])
                                stackRows(inputByOutputsElements).t
                            case (_: PMatrix, index: Int) =>
                                val inputByOutputsElements = inputsByOutputElements.map(t => t.toList(index).asInstanceOf[PMatrix])
                                val nInputElements = inputByOutputsElements.head.nRows * inputByOutputsElements.head.nCols
                                createMatrixFromElements(nInputElements, inputByOutputsElements.length, inputByOutputsElements.flatMap(_.elements))
                        })
                }).asInstanceOf[CartesianProductAndUpP[RT, DualTupleToPTuple[T]]]
                // unzip res, e.g. make ((A1, A2, A3), (B1, B2, B3)) to ((A1, B1), (A2, B2), (A3, B3))
                val finalRes = tWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (_: Any, id: Int) => res.map[[X] =>> Any]([U] => (x: U) => x.asInstanceOf[Tuple].toList(id))
                })
                finalRes.asInstanceOf[CartesianProductAndUpP[T, DualTupleToPTuple[RT]]]
            reversePlan(t)