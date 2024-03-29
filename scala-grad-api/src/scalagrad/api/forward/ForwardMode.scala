package scalagrad.api.forward

import scalagrad.api.dual.DualMatrixAlgebraDSL
import scalagrad.api.forward.dual.*
import scalagrad.api.matrixalgebra.derivative.DerivativeMatrixAlgebra
import scalagrad.api.matrixalgebra.{CreateOps, MatrixAlgebra, MatrixAlgebraDSL}
import scalagrad.api.{DualMode, Mode, dual}

import scala.annotation.{nowarn, targetName}
import scala.reflect.Typeable

object ForwardMode extends Mode:
    
    private def dualMode(alg: MatrixAlgebraDSL): ForwardDualMode[
        alg.Scalar, alg.ColumnVector, alg.RowVector, alg.Matrix,
    ] = ForwardDualMode(alg.innerAlgebra)

    @targetName("deriveS2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Scalar
    ):(alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar = 
        alg => x =>
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveCV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.ColumnVector = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveRV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.RowVector = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveM2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head
        
    @targetName("deriveS2CV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.ColumnVector = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveCV2CV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveRV2CV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveM2CV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.ColumnVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveS2RV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.RowVector = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveCV2RV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveRV2RV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveM2RV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.RowVector
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveS2M")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Scalar => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualScalar]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head

    @targetName("deriveCV2M")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.ColumnVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualColumnVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveRV2M")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.RowVector => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualRowVector]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveM2M")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => alg.Matrix => alg.Matrix
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x => 
            val mode = dualMode(alg)
            val df = mode.derive((t: Tuple1[mode.DualMatrix]) => f(mode.algebraDSL)(t.head))
            df(Tuple1(x)).head.asInstanceOf[alg.Matrix]

    @targetName("deriveSS2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar) = 
        alg => (s1, s2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(s1, s2)

    @targetName("deriveSCV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => alg.Scalar
        ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector) = 
            alg => (s, cv) =>
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(s, cv)

    @targetName("deriveCVS2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Scalar) => (alg.ColumnVector, alg.Scalar) = 
        alg => (cv, s) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(cv, s)

    @targetName("deriveCVCV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => (alg.ColumnVector, alg.ColumnVector) = 
        alg => (cv1, cv2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(cv1, cv2)

    @targetName("deriveMM2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Matrix) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => (alg.Matrix, alg.Matrix) = 
        alg => (m1, m2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(m1, m2)

    @targetName("deriveMCV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.ColumnVector) => (alg.Matrix, alg.ColumnVector) = 
        alg => (m, cv) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(m, cv)

    @targetName("deriveMRV2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.RowVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.RowVector) => (alg.Matrix, alg.RowVector) = 
        alg => (m, rv) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(m, rv)

    @targetName("deriveMS2S")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Scalar) => (alg.Matrix, alg.Scalar) = 
        alg => (m, s) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(m, s)

    @targetName("deriveSS2SS")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => ((alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar)) = 
        alg => (s1, s2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(s1, s2)

    @targetName("deriveSM2SM")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Matrix) => (alg.Scalar, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Matrix) => ((alg.Scalar, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Scalar, alg.Matrix), (alg.Matrix, alg.Matrix))]

    @targetName("deriveSM2MM")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.Matrix) => (alg.Matrix, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]

    @targetName("deriveCVCV2CVCV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.ColumnVector) => (alg.ColumnVector, alg.ColumnVector)
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]


    @targetName("deriveCVM2CVM")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.ColumnVector, alg.Matrix) => (alg.ColumnVector, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]


    @targetName("deriveRVRV2RVRV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.RowVector) => (alg.RowVector, alg.RowVector)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.RowVector) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]


    @targetName("deriveRVM2RVRV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.Matrix) => (alg.RowVector, alg.RowVector)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]

    @targetName("deriveRVM2RVM")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.RowVector, alg.Matrix) => (alg.RowVector, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.RowVector, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]

    @targetName("deriveMM2MM")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Matrix, alg.Matrix) => (alg.Matrix, alg.Matrix)
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => ((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix)) =
        alg => (x1, x2) => 
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(x1, x2).asInstanceOf[((alg.Matrix, alg.Matrix), (alg.Matrix, alg.Matrix))]

    @targetName("deriveSCV2SCV")
    override def derive[MA >: DualMatrixAlgebraDSL <: MatrixAlgebraDSL](
        f: (alg: MA) => (alg.Scalar, alg.ColumnVector) => (alg.Scalar, alg.ColumnVector)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.ColumnVector) => ((alg.Scalar, alg.ColumnVector), (alg.ColumnVector, alg.Matrix)) = 
        alg => (s, cv) =>
            val mode = dualMode(alg)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(s, cv).asInstanceOf[((alg.Scalar, alg.ColumnVector), (alg.ColumnVector, alg.Matrix))]
