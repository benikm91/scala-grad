package scalagrad.api.forward

import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.{CreateOps, MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.api.{DualMode, dual}

import scala.annotation.{nowarn, targetName}
import scala.reflect.Typeable

class ForwardDualMode[
    PScalar : Typeable, PColumnVector : Typeable, PRowVector : Typeable, PMatrix : Typeable,
](
    override val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
) extends DualMode[PScalar, PColumnVector, PRowVector, PMatrix]:
    
    override type  DScalar = PScalar
    override type  DColumnVector = PColumnVector
    override type  DRowVector = PRowVector
    override type  DMatrix = PMatrix

    override type DualScalar = DualNumberScalar[PScalar]
    override type DualColumnVector = DualNumberColumnVector[PColumnVector]
    override type DualRowVector = DualNumberRowVector[PRowVector]
    override type DualMatrix = DualNumberMatrix[PMatrix]

    override val derivativeMatrixAlgebra: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ] = new DualNumberDerivativeMatrixAlgebra(primaryMatrixAlgebra)

    val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val zeroIndices = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

    def deriveDualTuple2Scalar[T <: Tuple : DualTuple](f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T] = t =>
        val df = deriveDualTuple2DualTuple[T, Tuple1[DualScalar]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[DualTupleToPTuple[T]]
           
    def deriveDualTuple2ColumnVector[T <: Tuple : DualTuple](f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector] = t =>
        val df = deriveDualTuple2DualTuple[T, Tuple1[DualColumnVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]]
    
    def deriveDualTuple2RowVector[T <: Tuple : DualTuple](f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector] = t =>
        val df = deriveDualTuple2DualTuple[T, Tuple1[DualRowVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]]
    
    def deriveDualTuple2Matrix[T <: Tuple : DualTuple](f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix] = t =>
        val df = deriveDualTuple2DualTuple[T, Tuple1[DualMatrix]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]]
        
    def deriveScalar2DualTuple[RT <: Tuple : DualTuple](f: DualScalar => RT): PScalar => DualTupleToPTuple[RT] = t =>
        val df = deriveDualTuple2DualTuple[Tuple1[DualScalar], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    def deriveColumnVector2DualTuple[RT <: Tuple : DualTuple](f: DualColumnVector => RT): PColumnVector => DualTupleToPTuple[RT] = t =>
        val df = deriveDualTuple2DualTuple[Tuple1[DualColumnVector], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    def deriveRowVector2DualTuple[RT <: Tuple : DualTuple](f: DualRowVector => RT): PRowVector => DualTupleToPTuple[RT] = t =>
        val df = deriveDualTuple2DualTuple[Tuple1[DualRowVector], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    def deriveMatrix2DualTuple[RT <: Tuple : DualTuple](f: DualMatrix => RT): PMatrix => DualTupleToPTuple[RT] = t =>
        val df = deriveDualTuple2DualTuple[Tuple1[DualMatrix], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    def deriveDualTuple2DualTuple[T <: Tuple : DualTuple, RT <: Tuple : DualTuple](f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = inputs =>

        import derivativeMatrixAlgebra.*
        import primaryMatrixAlgebra.*

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
            zeroDuals.zip(indices).map[[X] =>> Any]([U] => (t: U) => 
                // map to Any lost type information, recover them with asInstanceOf
                t.asInstanceOf[(DualScalar, Int) | (DualColumnVector, Int) | (DualRowVector, Int) | (DualMatrix, Int)] match {
                    case (zeroDual: DualScalar, zeroDualIndex: Int) => 
                        // run forward plan for single scalar
                        val (top, bottom) = splitTopAndBottomAt(zeroDuals, zeroDualIndex)
                        val oneHotDual = createDualScalar(zeroDual.v, one)
                        val scalar2Outputs = f(appendTopAndBottomWith(top, oneHotDual, bottom).asInstanceOf[T])
                        // map over outputs to extract derivatives
                        scalar2Outputs.map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[DualScalar | DualColumnVector | DualRowVector | DualMatrix] match {
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
                        representation.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(DualScalar, Int) | (DualColumnVector, Int) | (DualRowVector, Int) | (DualMatrix, Int)] match {
                                case (_: DualScalar, outputIndex: Int) => 
                                    val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualScalar].dv)
                                    createColumnVectorFromElements(elements2dOutput)
                                case (_: DualColumnVector, outputIndex: Int) => 
                                    val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualColumnVector].dv)
                                    // tranpose stacked columns as one row represent derivatives for one input element
                                    stackColumns(elements2dOutput).t
                                case (_: DualRowVector, outputIndex: Int) => 
                                    val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualRowVector].dv)
                                    stackRows(elements2dOutput)
                                case (_: DualMatrix, outputIndex: Int) =>
                                    val elements2dOutput = elements2Outputs.map(t => t.toList(outputIndex).asInstanceOf[DualMatrix].dv)
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
                        representation.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(DualScalar, Int) | (DualColumnVector, Int) | (DualRowVector, Int) | (DualMatrix, Int)] match {
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
                        elements2Outputs.head.zip(zeroIndices).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(DualScalar, Int) | (DualColumnVector, Int) | (DualRowVector, Int) | (DualMatrix, Int)] match {
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
