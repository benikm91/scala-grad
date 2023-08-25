package scalagrad.api

import scala.runtime.Tuples
import scalagrad.api.dual
import scalagrad.api.dual.DualMatrixAlgebra
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.spire.trig.DualScalarIsTrig
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.dual.DualMatrixAlgebraDSL
import scala.reflect.Typeable
import scala.annotation.targetName

trait ModeOE2E:
    
    @targetName("deriveS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Scalar
    ):(alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar
                
    @targetName("deriveCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.ColumnVector

    @targetName("deriveRV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.RowVector

    @targetName("deriveM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix
        
    @targetName("deriveS2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.ColumnVector

    @targetName("deriveCV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix

    @targetName("deriveRV2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix

    @targetName("deriveM2CV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix

    @targetName("deriveS2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.RowVector

    @targetName("deriveCV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix

    @targetName("deriveRV2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix

    @targetName("deriveM2RV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix

    @targetName("deriveS2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Matrix

    @targetName("deriveCV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix

    @targetName("deriveRV2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix

    @targetName("deriveM2M")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix

trait ModeAB2S:

    @targetName("deriveSS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)

    @targetName("deriveCVS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Scalar) => (alg.ColumnVector, alg.Scalar)

    @targetName("deriveCVCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => (alg.ColumnVector, alg.ColumnVector)

    @targetName("deriveMM2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Matrix) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => (alg.Matrix, alg.Matrix)

    @targetName("deriveMCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.ColumnVector) => (alg.Matrix, alg.ColumnVector)

    @targetName("deriveMRV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.RowVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.RowVector) => (alg.Matrix, alg.RowVector)

    @targetName("deriveMS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Scalar) => (alg.Matrix, alg.Scalar)

trait ModeAB2CD:

    @targetName("deriveSS2SS")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => ((alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar))

    @targetName("deriveSM2SM")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Matrix) => (alg.Scalar, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Matrix) => ((alg.Scalar, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveSM2MM")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Matrix) => (alg.Matrix, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveCVCV2CVCV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.ColumnVector) => (alg.ColumnVector, alg.ColumnVector)
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveCVM2CVM")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Matrix) => (alg.ColumnVector, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveRVRV2RVRV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.RowVector) => (alg.RowVector, alg.RowVector)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.RowVector) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveRVM2RVRV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.Matrix) => (alg.RowVector, alg.RowVector)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveRVM2RVM")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.Matrix) => (alg.RowVector, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))

    @targetName("deriveMM2MM")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Matrix) => (alg.Matrix, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))



trait ModeO extends ModeOE2E with ModeAB2S with ModeAB2CD

abstract class Mode[
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
    DualScalar <: dual.DualScalar[PScalar, DScalar] : Typeable, 
    DualColumnVector <: dual.DualColumnVector[PColumnVector, DColumnVector] : Typeable,
    DualRowVector <: dual.DualRowVector[PRowVector, DRowVector] : Typeable,
    DualMatrix <: dual.DualMatrix[PMatrix, DMatrix] : Typeable,
    DerivativeMatrixAlgebraT <: DerivativeMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ]
](
    val primaryMatrixAlgebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
    val derivativeMatrixAlgebra: DerivativeMatrixAlgebraT
):

    import primaryMatrixAlgebra.given

    val algebra = DualMatrixAlgebra[
        PScalar, PColumnVector, PRowVector, PMatrix, 
        DScalar, DColumnVector, DRowVector, DMatrix, 
        DualScalar, DualColumnVector, DualRowVector, DualMatrix
    ](
        primaryMatrixAlgebra,
        derivativeMatrixAlgebra,
    )

    val stE = summon[Typeable[DualScalar]]
    val cvtE = summon[Typeable[DualColumnVector]]
    val rvtE = summon[Typeable[DualRowVector]]
    val mtE = summon[Typeable[DualMatrix]]

    val algebraDSL = new DualMatrixAlgebraDSL {

        override given st: Typeable[Scalar] = stE
        override given cvt: Typeable[ColumnVector] = cvtE
        override given rvt: Typeable[RowVector] = rvtE
        override given mt: Typeable[Matrix] = mtE

        override type PrimaryScalar = PScalar
        override type PrimaryColumnVector = PColumnVector
        override type PrimaryRowVector = PRowVector
        override type PrimaryMatrix = PMatrix
        override type DerivativeScalar = DScalar
        override type DerivativeColumnVector = DColumnVector
        override type DerivativeRowVector = DRowVector
        override type DerivativeMatrix = DMatrix
        override type Scalar = DualScalar
        override type ColumnVector = DualColumnVector
        override type RowVector = DualRowVector
        override type Matrix = DualMatrix
        override val innerAlgebra = algebra
    }

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

    type UpPByColumnVectorTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByColumnVector[head] *: UpPByColumnVectorTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpPByColumnVector[T] = T match
        case PScalar       => PColumnVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByRowVectorTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByRowVector[head] *: UpPByRowVectorTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpPByRowVector[T] = T match
        case PScalar       => PRowVector
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type UpPByMatrixTuple[T <: Tuple] <: Tuple = T match
        case (head *: tail) => UpPByMatrix[head] *: UpPByMatrixTuple[tail]
        case EmptyTuple     => EmptyTuple

    type UpPByMatrix[T] = T match
        case PScalar       => PMatrix
        case PColumnVector => PMatrix
        case PRowVector    => PMatrix
        case PMatrix       => PMatrix

    type CopyAndUpP = [T <: Tuple] =>> [X] =>> X match
        case DualScalar => T
        case DualColumnVector => UpPByColumnVectorTuple[T]
        case DualRowVector => UpPByRowVectorTuple[T]
        case DualMatrix => UpPByMatrixTuple[T]

    type CartesianProductAndUpP[T1 <: Tuple, T2 <: Tuple] = Tuple.Map[T1, CopyAndUpP[T2]]

