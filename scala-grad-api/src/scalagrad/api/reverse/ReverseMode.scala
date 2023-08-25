package scalagrad.api.reverse

import scalagrad.api.Mode
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.reverse.dual.*
import scalagrad.api.reverse.delta.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.OneOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.reverse.eval.Eval
import scalagrad.api.reverse.DualDeltaDerivativeMatrixAlgebra
import scala.reflect.Typeable
import scala.annotation.targetName
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.ScalaGrad

object ReverseMode:
    
    @targetName("deriveS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar = 
        alg => s => 
            val mode = new ReverseMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(s)).head

    @targetName("deriveCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.ColumnVector = 
        alg => cv => 
            val mode = new ReverseMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(cv)).head

    @targetName("deriveRV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.RowVector =
        alg => rv =>
            val mode = new ReverseMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(rv)).head

    @targetName("deriveM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = new ReverseMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
    
    @targetName("deriveS2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.ColumnVector = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2ColumnVector[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2ColumnVector[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2ColumnVector[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveM2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2ColumnVector[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveS2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.RowVector = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveM2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveS2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveM2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ReverseMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveSS2SS")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => ((alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar)) = 
        alg => (s1, s2) => 
            val mode = ReverseMode(alg.innerAlgebra)
            import mode.given
            ScalaGrad.derive(f(mode.algebraDSL))(s1, s2)

    @targetName("deriveSCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector) = 
        alg => (s, cv) =>
            val mode = new ReverseMode(alg.innerAlgebra)
            import mode.given
            ScalaGrad.derive(f(mode.algebraDSL))(s, cv)
            
    @targetName("deriveCVMCVM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) => (alg.ColumnVector, alg.Matrix, alg.ColumnVector, alg.Matrix) = 
        alg => (cv1, m1, cv2, m2) =>
            val mode = new ReverseMode(alg.innerAlgebra)
            import mode.given
            ScalaGrad.derive(f(mode.algebraDSL))(cv1, m1, cv2, m2)
            

class ReverseMode[
    PScalar : Typeable, PColumnVector : Typeable, PRowVector : Typeable, PMatrix : Typeable,
](
    primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
) extends Mode[
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
 
        import primaryMatrixAlgebra.*
        import derivativeMatrixAlgebra.*
            
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
                        val inputsByOutputElements = for (i <- 0 until x.v.length) yield { 
                            val res = eval.evalColumnVector(oneHotColumnVector(x.v.length, i), x.delta)
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
                        val inputsByOutputElements = for (i <- 0 until x.v.length) yield { 
                            val res = eval.evalRowVector(oneHotRowVector(x.v.length, i), x.delta)
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