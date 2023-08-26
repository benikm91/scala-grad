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
import scalagrad.api.forward.ForwardDualMode
import scalagrad.api.reverse.ReverseDualMode

trait Mode:
    
    @targetName("deriveS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar

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

    @targetName("deriveSS2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)

    @targetName("deriveSCV2S")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => alg.Scalar
        ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector)

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

    @targetName("deriveSCV2SCV")
    def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => ((alg.Scalar, alg.ColumnVector), (alg.ColumnVector, alg.Matrix))
