package scalagrad.api.forward

import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.{CreateOps, MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.api.{DualMode, dual}

import scala.annotation.{nowarn, targetName}
import scala.reflect.TypeTest
import scalagrad.api.dual.DualMatrixAlgebra

class ForwardDualMode[
    PScalar, PColumnVector, PRowVector, PMatrix,
](
    override val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
)(
    // We have runtime type test to check if the type is PScalar | PColumnVector | PRowVector | PMatrix
    using 
    TypeTest[PScalar | PColumnVector | PRowVector | PMatrix, PScalar],
    TypeTest[PScalar | PColumnVector | PRowVector | PMatrix, PColumnVector],
    TypeTest[PScalar | PColumnVector | PRowVector | PMatrix, PRowVector],
    TypeTest[PScalar | PColumnVector | PRowVector | PMatrix, PMatrix],
) extends DualMode(
    DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        PScalar, PColumnVector, PRowVector, PMatrix,  
        DualNumberScalar[PScalar],
        DualNumberColumnVector[PColumnVector],
        DualNumberRowVector[PRowVector],
        DualNumberMatrix[PMatrix],
    ](
        primaryMatrixAlgebra,
        new DualNumberDerivativeMatrixAlgebra(primaryMatrixAlgebra),
    )
):
    
    override given dualScalarTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualScalar] = 
        new TypeTest {
            def unapply(x: DualScalar | DualColumnVector | DualRowVector | DualMatrix): Option[x.type & DualScalar] = 
                x match {
                    case _: DualScalar => Some(x.asInstanceOf[x.type & DualScalar])
                    case _ => None
                }
        }

    override given dualColumnVectorTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualColumnVector] = 
        new TypeTest {
            def unapply(x: DualScalar | DualColumnVector | DualRowVector | DualMatrix): Option[x.type & DualColumnVector] = 
                x match {
                    case _: DualColumnVector => Some(x.asInstanceOf[x.type & DualColumnVector])
                    case _ => None
                }
        }

    override given dualRowVectorTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualRowVector] = 
        new TypeTest {
            def unapply(x: DualScalar | DualColumnVector | DualRowVector | DualMatrix): Option[x.type & DualRowVector] = 
                x match {
                    case _: DualRowVector => Some(x.asInstanceOf[x.type & DualRowVector])
                    case _ => None
                }
        }
    
    override given dualMatrixTest: TypeTest[DualScalar | DualColumnVector | DualRowVector | DualMatrix, DualMatrix] = 
        new TypeTest {
            def unapply(x: DualScalar | DualColumnVector | DualRowVector | DualMatrix): Option[x.type & DualMatrix] = 
                x match {
                    case _: DualMatrix => Some(x.asInstanceOf[x.type & DualMatrix])
                    case _ => None
                }
        }

    val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val zeroIndices = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

    @targetName("deriveScalar2Scalar")
    def derive(f: DualScalar => DualScalar): PScalar => PScalar = x =>
        val df = this.derive((t: Tuple1[this.DualScalar]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveColumnVector2Scalar")
    def derive(f: DualColumnVector => DualScalar): PColumnVector => PColumnVector = x =>
        val df = this.derive((t: Tuple1[this.DualColumnVector]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveRowVector2Scalar")
    def derive(f: DualRowVector => DualScalar): PRowVector => PRowVector = x =>
        val df = this.derive((t: Tuple1[this.DualRowVector]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveMatrix2Scalar")
    def derive(f: DualMatrix => DualScalar): PMatrix => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualMatrix]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveScalar2ColumnVector")
    def derive(f: DualScalar => DualColumnVector): PScalar => PColumnVector = x =>
        val df = this.derive((t: Tuple1[this.DualScalar]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveColumnVector2ColumnVector")
    def derive(f: DualColumnVector => DualColumnVector): PColumnVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualColumnVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveRowVector2ColumnVector")
    def derive(f: DualRowVector => DualColumnVector): PRowVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualRowVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveMatrix2ColumnVector")
    def derive(f: DualMatrix => DualColumnVector): PMatrix => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualMatrix]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveScalar2RowVector")
    def derive(f: DualScalar => DualRowVector): PScalar => PRowVector = x =>
        val df = this.derive((t: Tuple1[this.DualScalar]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveColumnVector2RowVector")
    def derive(f: DualColumnVector => DualRowVector): PColumnVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualColumnVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveRowVector2RowVector")
    def derive(f: DualRowVector => DualRowVector): PRowVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualRowVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveMatrixVector2MatrixVector")
    def derive(f: DualMatrix => DualMatrix): PMatrix => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualMatrix]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveScalar2Matrix")
    def derive(f: DualScalar => DualMatrix): PScalar => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualScalar]) => f(t.head))
        df(Tuple1(x)).head

    @targetName("deriveColumnVector2Matrix")
    def derive(f: DualColumnVector => DualMatrix): PColumnVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualColumnVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveRowVector2Matrix")
    def derive(f: DualRowVector => DualMatrix): PRowVector => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualRowVector]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveMatrix2Matrix")
    def derive(f: DualMatrix => DualMatrix): PMatrix => PMatrix = x =>
        val df = this.derive((t: Tuple1[this.DualMatrix]) => f(t.head))
        df(Tuple1(x)).head.asInstanceOf[PMatrix]

    @targetName("deriveDualTuple2Scalar")
    override def derive[T <: Tuple : DualTuple](f: T => DualScalar): DualTupleToPTuple[T] => DualTupleToPTuple[T] = t =>
        val df = derive[T, Tuple1[DualScalar]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[DualTupleToPTuple[T]]
           
    @targetName("deriveDualTuple2ColumnVector")
    override def derive[T <: Tuple : DualTuple](f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector] = t =>
        val df = derive[T, Tuple1[DualColumnVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]]
    
    @targetName("deriveDualTuple2RowVector")
    override def derive[T <: Tuple : DualTuple](f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector] = t =>
        val df = derive[T, Tuple1[DualRowVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]]
    
    @targetName("deriveDualTuple2Matrix")
    override def derive[T <: Tuple : DualTuple](f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix] = t =>
        val df = derive[T, Tuple1[DualMatrix]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any]([T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]]
        
    @targetName("deriveScalar2DualTuple")
    override def derive[RT <: Tuple : DualTuple](f: DualScalar => RT): PScalar => DualTupleToPTuple[RT] = t =>
        val df = derive[Tuple1[DualScalar], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    @targetName("deriveColumnVector2DualTuple")
    override def derive[RT <: Tuple : DualTuple](f: DualColumnVector => RT): PColumnVector => DualTupleToPTuple[RT] = t =>
        val df = derive[Tuple1[DualColumnVector], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    @targetName("deriveRowVector2DualTuple")
    override def derive[RT <: Tuple : DualTuple](f: DualRowVector => RT): PRowVector => DualTupleToPTuple[RT] = t =>
        val df = derive[Tuple1[DualRowVector], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    @targetName("deriveMatrix2DualTuple")
    override def derive[RT <: Tuple : DualTuple](f: DualMatrix => RT): PMatrix => DualTupleToPTuple[RT] = t =>
        val df = derive[Tuple1[DualMatrix], RT](t => f(t.head))
        df(Tuple1(t)).asInstanceOf[Tuple1[DualTupleToPTuple[RT]]].head
    
    @targetName("deriveDualTuple2DualTuple")
    override def derive[T <: Tuple : DualTuple, RT <: Tuple : DualTuple](f: T => RT): DualTupleToPTuple[T] => CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = inputs =>

        import derivativeMatrixAlgebra.*
        import primaryMatrixAlgebra.*

        def toZeroDuals[T <: Tuple : DualTuple](t: DualTupleToPTuple[T]): T = 
            t.map[[X] =>> Any]([T] => (t: T) => 
                // map to Any lost type information, recover them with asInstanceOf
                t.asInstanceOf[PScalar | PColumnVector | PRowVector | PMatrix] match {
                    case s: PScalar => createDualScalar(s, dZeroOps.zeroScalar)
                    case cv: PColumnVector => createDualColumnVector(cv, dZeroOps.zeroColumnVector(cv.length))
                    case rv: PRowVector => createDualRowVector(rv, dZeroOps.zeroRowVector(rv.length))
                    case m: PMatrix => createDualMatrix(m, dZeroOps.zeroMatrix(m.nRows, m.nCols))
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
