package scalagrad.api.forward

import scalagrad.api.Mode
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scala.reflect.Typeable
import scala.annotation.nowarn

class ForwardMode[
    PScalar : Typeable, PColumnVector : Typeable, PRowVector : Typeable, PMatrix : Typeable,
](
    primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
) extends Mode[
    PScalar, PColumnVector, PRowVector, PMatrix,
    PScalar, PColumnVector, PRowVector, PMatrix,
    DualNumberScalar[PScalar], 
    DualNumberColumnVector[PColumnVector], 
    DualNumberRowVector[PRowVector], 
    DualNumberMatrix[PMatrix],
    DualNumberDerivativeMatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
](
    primaryMatrixAlgebra,
    DualNumberDerivativeMatrixAlgebra(primaryMatrixAlgebra)
):
    type DualScalar = DualNumberScalar[PScalar]
    type DualColumnVector = DualNumberColumnVector[PColumnVector]
    type DualRowVector = DualNumberRowVector[PRowVector]
    type DualMatrix = DualNumberMatrix[PMatrix]

    import primaryMatrixAlgebra.*
    import derivativeMatrixAlgebra.*

    val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val zeroIndices = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

    // TODO can we move this to Mode?
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

        @nowarn
        override def derive(f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = inputs =>

            def toZeroDuals[T <: Tuple : DualTuple](t: DualTupleToPTuple[T]): T = 
                t.map[[X] =>> Any]([T] => (t: T) => t match {
                    case x: PScalar => createDualScalar(x, dZeroOps.zeroScalar)
                    case x: PColumnVector => createDualColumnVector(x, dZeroOps.zeroColumnVector(x.length))
                    case x: PRowVector => createDualRowVector(x, dZeroOps.zeroRowVector(x.length))
                    case x: PMatrix => createDualMatrix(x, dZeroOps.zeroMatrix(x.nRows, x.nCols))
                }).asInstanceOf[T]
                
            def forwardPlan(zeroDuals: T) = 
                def splitTopAndBottomAt(duals: T, pos: Int): (Tuple, Tuple) = 
                    val (topWithX, bottom) = duals.splitAt(pos)
                    (topWithX.asInstanceOf[NonEmptyTuple].init, bottom)
                def appendTopAndBottomWith(top: Tuple, newX: Any, bottom: Tuple) = 
                    top ++ (newX *: bottom)
                // run forward plan for each input by mapping over all inputs (zeroDuals)
                zeroDuals.zip(indices).map[[X] =>> Any]([U] => (t: U) => t match {
                    case (zeroDual: DualScalar, zeroDualIndex: Int) => 
                        // run forward plan for single scalar
                        val (top, bottom) = splitTopAndBottomAt(zeroDuals, zeroDualIndex)
                        val oneHotDual = createDualScalar(zeroDual.v, one)
                        val scalar2Outputs = f(appendTopAndBottomWith(top, oneHotDual, bottom).asInstanceOf[T])
                        // map over outputs to extract derivatives
                        scalar2Outputs.map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case x: DualScalar => x.dv
                            case x: DualColumnVector => x.dv
                            case x: DualRowVector => x.dv
                            case x: DualMatrix => x.dv
                        })
                    case (zeroDual: DualColumnVector, zeroDualIndex: Int) => 
                        // run forward plan for elements in column vector
                        val (top, bottom) = splitTopAndBottomAt(zeroDuals, zeroDualIndex)
                        val elements2Outputs = (
                            for (i <- 0 until zeroDual.v.length)
                                yield {
                                    val oneHotDual = createDualColumnVector(zeroDual.v, oneHotColumnVector(zeroDual.v.length, i))
                                    f(appendTopAndBottomWith(top, oneHotDual, bottom).asInstanceOf[T])
                                }
                        ).toVector
                        // map over output structure to combine elements from each output position into a single structure (e.g. Matrix)
                        val representation = elements2Outputs.head
                        representation.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (_: DualNumberScalar[PScalar], outputIndex: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualNumberScalar[PScalar]].dv)
                                createColumnVectorFromElements(elements2dOutput)
                            case (_: DualNumberColumnVector[PColumnVector], outputIndex: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualNumberColumnVector[PColumnVector]].dv)
                                // tranpose stacked columns as one row represent derivatives for one input element
                                stackColumns(elements2dOutput).t
                            case (_: DualNumberRowVector[PRowVector], outputIndex: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualNumberRowVector[PRowVector]].dv)
                                stackRows(elements2dOutput)
                            case (_: DualNumberMatrix[PMatrix], outputIndex: Int) =>
                                val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualNumberMatrix[PMatrix]].dv)
                                val nElements = elements2dOutput.map(m => m.nRows * m.nCols).sum
                                createMatrixFromElements(elements2dOutput.length, nElements / elements2dOutput.length, elements2dOutput.map(_.elements).transpose.flatten)
                        })
                    case (zeroDual: DualRowVector, zeroDualIndex: Int) => 
                        // run forward plan for elements in row vector
                        val (top, bottom) = splitTopAndBottomAt(zeroDuals, zeroDualIndex)
                        val elements2Outputs = (
                            for (i <- 0 until zeroDual.v.length)
                                yield {
                                    val oneHotDual = createDualRowVector(zeroDual.v, oneHotRowVector(zeroDual.v.length, i))
                                    f(appendTopAndBottomWith(top, oneHotDual, bottom).asInstanceOf[T])
                                }
                        ).toVector
                        // map over output structure to combine elements from each output position into a single structure (e.g. Matrix)
                        val representation = elements2Outputs.head 
                        representation.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (_: DualNumberScalar[PScalar], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberScalar[PScalar]].dv)
                                createRowVectorFromElements(elements2dOutput)
                            case (_: DualNumberColumnVector[PColumnVector], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberColumnVector[PColumnVector]].dv)
                                // tranpose stacked columns as one row represents derivatives for one input element
                                stackColumns(elements2dOutput).t
                            case (_: DualNumberRowVector[PRowVector], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberRowVector[PRowVector]].dv)
                                stackRows(elements2dOutput)
                            case (_: DualNumberMatrix[PMatrix], index: Int) =>
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberMatrix[PMatrix]].dv)
                                val nElements = elements2dOutput.map(m => m.nRows * m.nCols).sum
                                createMatrixFromElements(elements2dOutput.length, nElements / elements2dOutput.length, elements2dOutput.map(_.elements).transpose.flatten)
                        })
                    case (zeroDual: DualMatrix, zeroDualIndex: Int) => 
                        // run forward plan for elements in matrix
                        val (top, bottom) = splitTopAndBottomAt(zeroDuals, zeroDualIndex)
                        val elements2Outputs = (
                            for (i <- 0 until zeroDual.v.nRows * zeroDual.v.nCols)
                                yield {
                                    val oneHotDual = createDualMatrix(zeroDual.v, oneHotMatrix(zeroDual.v.nRows, zeroDual.v.nCols, i))
                                    f(appendTopAndBottomWith(top, oneHotDual, bottom).asInstanceOf[T])
                                }
                        ).toVector
                        // map over output structure to combine elements from each output position into a single structure (e.g. Matrix)
                        elements2Outputs.head.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => t2 match {
                            case (_: DualNumberScalar[PScalar], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberScalar[PScalar]].dv)
                                createMatrixFromElements(zeroDual.v.nRows, zeroDual.v.nCols, elements2dOutput)
                            case (_: DualNumberColumnVector[PColumnVector], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberColumnVector[PColumnVector]].dv)
                                // tranpose stacked columns as one row represent derivatives for one input element
                                stackColumns(elements2dOutput).t
                            case (_: DualNumberRowVector[PRowVector], index: Int) => 
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberRowVector[PRowVector]].dv)
                                // fix "wrong" order due to oneHotEncoding of matrix being row-major wise.
                                val elements2dOutputFixed = elements2dOutput.grouped(zeroDual.v.nCols).toVector.transpose.flatten
                                stackRows(elements2dOutputFixed)
                            case (_: DualNumberMatrix[PMatrix], index: Int) =>
                                val elements2dOutput = elements2Outputs.map(t => t.toList(index).asInstanceOf[DualNumberMatrix[PMatrix]].dv)
                                val nElements = elements2dOutput.map(m => m.nRows * m.nCols).sum
                                createMatrixFromElements(elements2dOutput.length, nElements / elements2dOutput.length, elements2dOutput.map(_.elements).transpose.flatten)
                        })
                }).asInstanceOf[CartesianProductAndUpP[T, DualTupleToPTuple[RT]]]
            forwardPlan(toZeroDuals(inputs))
