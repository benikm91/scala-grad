package scalagrad.api.reverse

import scalagrad.api.DualMode
import scalagrad.api.dual.{DualMatrixAlgebra, DualMatrixAlgebraDSL}
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.{CreateOps, MatrixAlgebra, MatrixAlgebraDSL, OneOps}
import scalagrad.api.reverse.DualDeltaDerivativeMatrixAlgebra
import scalagrad.api.reverse.delta.*
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.eval.Eval

import scala.annotation.targetName
import scala.reflect.TypeTest
import scalagrad.api.dual.DualMatrixAlgebra

class ReverseDualMode[
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
)  extends DualMode(
    DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
        DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], 
        DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], 
        DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix], 
        DualDeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix], 
        DualDeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix], 
        DualDeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix], 
        DualDeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]
 
    ](
        primaryMatrixAlgebra,
        DualDeltaDerivativeMatrixAlgebra(),
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

    val eval = Eval[PScalar, PColumnVector, PRowVector, PMatrix](primaryMatrixAlgebra)

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
        df(t).map[[X] =>> Any](
            [T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head
        ).asInstanceOf[DualTupleToPTuple[T]]
           
    @targetName("deriveDualTuple2ColumnVector")
    override def derive[T <: Tuple : DualTuple](f: T => DualColumnVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector] = t =>
        val df = derive[T, Tuple1[DualColumnVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any](
            [T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head
        ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByColumnVector]]
    
    @targetName("deriveDualTuple2RowVector")
    override def derive[T <: Tuple : DualTuple](f: T => DualRowVector): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByRowVector] = t =>
        val df = derive[T, Tuple1[DualRowVector]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any](
            [T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head
        ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByRowVector]]
    
    @targetName("deriveDualTuple2Matrix")
    override def derive[T <: Tuple : DualTuple](f: T => DualMatrix): DualTupleToPTuple[T] => Tuple.Map[DualTupleToPTuple[T], UpPByMatrix] = t =>
        val df = derive[T, Tuple1[DualMatrix]](t => Tuple1(f(t)))
        df(t).map[[X] =>> Any](
            [T] => (e: T) => e.asInstanceOf[Tuple1[Any]].head
        ).asInstanceOf[Tuple.Map[DualTupleToPTuple[T], UpPByMatrix]]
        
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

        val ids = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

        import derivativeMatrixAlgebra.*
        import primaryMatrixAlgebra.*
        
        def reversePlan(t: DualTupleToPTuple[T]): CartesianProductAndUpP[T, DualTupleToPTuple[RT]] = 
            val tWithIndex = t.zip(ids)
            val output = f(
                tWithIndex.map[[X] =>> Any]([U] => (t: U) =>
                    // map to Any lost type information, recover them with asInstanceOf
                    t.asInstanceOf[(PScalar, Int) | (PColumnVector, Int) | (PRowVector, Int) | (PMatrix, Int)] match {
                        case (s: PScalar, id: Int) => createDualScalar(s, DeltaScalar.Val(0, DeltaScalar.Input(id)))
                        case (cv: PColumnVector, id: Int) => createDualColumnVector(cv, DeltaColumnVector.Val(0, DeltaColumnVector.Input(id)))
                        case (rv: PRowVector, id: Int) => createDualRowVector(rv, DeltaRowVector.Val(0, DeltaRowVector.Input(id)))
                        case (m: PMatrix, id: Int) => createDualMatrix(m, DeltaMatrix.Val(0, DeltaMatrix.Input(id)))
                    }).asInstanceOf[T]
            )
            def extractResults(res: eval.Results): DualTupleToPTuple[RT] =
                tWithIndex.map[[X] =>> Any]([U] => (t: U) => 
                    // map to Any lost type information, recover them with asInstanceOf
                    t.asInstanceOf[(PScalar, Int) | (PColumnVector, Int) | (PRowVector, Int) | (PMatrix, Int)] match {
                        case (_: PScalar, id: Int) => res.scalars.get(id).getOrElse(zeroScalar)
                        case (cv: PColumnVector, id: Int) => res.columnVectors.get(id).getOrElse(zeroColumnVector(cv.length))
                        case (rv: PRowVector, id: Int) => res.rowVectors.get(id).getOrElse(zeroRowVector(rv.length))
                        case (m: PMatrix, id: Int) => res.matrices.get(id).getOrElse(zeroMatrix(m.nRows, m.nCols))
                    }).asInstanceOf[DualTupleToPTuple[RT]]
            // map over outputs
            val res = output.map[[X] =>> Any]([U] => (t: U) => 
                // map to Any lost type information, recover them with asInstanceOf
                t.asInstanceOf[DualScalar | DualColumnVector | DualRowVector | DualMatrix] match {
                    case x: (DualScalar) => 
                        // run reverse plan for scalar
                        val res = eval.evalScalar(one, x.delta)
                        extractResults(res)
                    case x: (DualColumnVector) =>
                        // run reverse plan for column vector
                        val inputsByOutputElements = for (i <- 0 until x.v.length) yield { 
                            val res = eval.evalColumnVector(oneHotColumnVector(x.v.length, i), x.delta)
                            extractResults(res)
                        }
                        // map over input
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(PScalar, Int) | (PColumnVector, Int) | (PRowVector, Int) | (PMatrix, Int)] match {
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
                    case x: (DualRowVector) => 
                        // run reverse plan for row vector
                        val inputsByOutputElements = for (i <- 0 until x.v.length) yield { 
                            val res = eval.evalRowVector(oneHotRowVector(x.v.length, i), x.delta)
                            extractResults(res)
                        }
                        // map over inputs
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(PScalar, Int) | (PColumnVector, Int) | (PRowVector, Int) | (PMatrix, Int)] match {
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
                    case x: (DualMatrix) => 
                        // run reverse plan for matrix
                        val inputsByOutputElements = for (i <- 0 until x.v.nRows * x.v.nCols) yield {
                            val res = eval.evalMatrix(oneHotMatrix(x.v.nRows, x.v.nCols, i), x.delta)
                            extractResults(res)
                        }
                        // map over inputs
                        inputsByOutputElements.head.zip(ids).map[[X] =>> Any]([U] => (t2: U) => 
                            // map to Any lost type information, recover them with asInstanceOf
                            t2.asInstanceOf[(PScalar, Int) | (PColumnVector, Int) | (PRowVector, Int) | (PMatrix, Int)] match {
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
        reversePlan(inputs)