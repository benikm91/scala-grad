package scalagrad.api.forward

import scalagrad.api.Mode
import scalagrad.api.DeriverFromTo
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.CreateOps
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scala.reflect.Typeable
import scala.annotation.nowarn
import scala.annotation.targetName
import scalagrad.api.ScalaGrad

import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebraDSL

object ForwardMode:

    @targetName("deriveS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Scalar
    ):(alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar = 
        alg => x =>
            val mode = new ForwardMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
                
    @targetName("deriveCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.ColumnVector = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveRV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.RowVector = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Scalar[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
        
    @targetName("deriveS2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.ColumnVector = 
            alg => x => 
                val mode = ForwardMode(alg.innerAlgebra)
                mode.tuple2ColumnVector[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
            alg => x => 
                val mode = ForwardMode(alg.innerAlgebra)
                mode.tuple2ColumnVector[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
                .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
            alg => x => 
                val mode = ForwardMode(alg.innerAlgebra)
                mode.tuple2ColumnVector[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
                .asInstanceOf[alg.Matrix]

    @targetName("deriveM2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
            alg => x => 
                val mode = ForwardMode(alg.innerAlgebra)
                mode.tuple2ColumnVector[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
                .asInstanceOf[alg.Matrix]

    @targetName("deriveS2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.RowVector = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveM2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2RowVector[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveS2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head

    @targetName("deriveCV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualColumnVector]].derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveRV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualRowVector]].derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveM2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = ForwardMode(alg.innerAlgebra)
            mode.tuple2Matrix[Tuple1[mode.DualMatrix]].derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))(Tuple1(x)).head
            .asInstanceOf[alg.Matrix]

    @targetName("deriveSS2SS")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => ((alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar)) = 
        alg => (s1, s2) =>
            val mode = ForwardMode(alg.innerAlgebra)
            import mode.tuple2Tuple
            ScalaGrad.derive(f(mode.algebraDSL))(s1, s2)

    @targetName("deriveSCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => alg.Scalar
        ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector) = 
            alg => (s, cv) =>
            val mode = new ForwardMode(alg.innerAlgebra)
            import mode.given
            ScalaGrad.derive(f(mode.algebraDSL))(s, cv)

    @targetName("deriveSCV2SCV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => ((alg.Scalar, alg.ColumnVector), (alg.ColumnVector, alg.Matrix)) = 
        alg => (s, cv) =>
            val mode = new ForwardMode(alg.innerAlgebra)
            import mode.given
            ScalaGrad.derive(f(mode.algebraDSL))(s, cv).asInstanceOf[((alg.Scalar, alg.ColumnVector), (alg.ColumnVector, alg.Matrix))]

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
