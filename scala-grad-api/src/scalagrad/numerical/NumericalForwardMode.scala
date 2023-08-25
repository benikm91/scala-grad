package scalagrad.numerical

import scalagrad.api.DeriverFromTo
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scala.reflect.Typeable
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scala.annotation.targetName

object NumericalForwardMode:

    @targetName("deriveS2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Scalar => alg.Scalar = 
        alg => x =>
            approxScalar2ScalarWith(alg, 1e-6).derive(f(alg))(x)

    @targetName("deriveCV2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.ColumnVector => alg.ColumnVector = 
        alg => x =>
            approxColumnVector2ScalarWith(alg, 1e-6).derive(f(alg))(x)

    @targetName("deriveRV2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => alg.RowVector => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.RowVector => alg.RowVector = 
        alg => x =>
            approxRowVector2ScalarWith(alg, 1e-6).derive(f(alg))(x)

    @targetName("deriveM2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => alg.Matrix => alg.Matrix = 
        alg => x =>
            approxMatrix2ScalarWith(alg, 1e-6).derive(f(alg))(x)

    @targetName("deriveSS2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar) = 
        alg => (s1, s2) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((s1, s2))

    @targetName("deriveCVS2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.Scalar) => (alg.ColumnVector, alg.Scalar) = 
        alg => (cv, s) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((cv, s))

    @targetName("deriveCVCV2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.ColumnVector, alg.ColumnVector) => (alg.ColumnVector, alg.ColumnVector) = 
        alg => (cv1, cv2) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((cv1, cv2))

    @targetName("deriveMM2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Matrix) => (alg.Matrix, alg.Matrix) = 
        alg => (m1, m2) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((m1, m2))

    @targetName("deriveMCV2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.ColumnVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.ColumnVector) => (alg.Matrix, alg.ColumnVector) = 
        alg => (m, cv) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((m, cv))

    @targetName("deriveMRV2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.RowVector) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.RowVector) => (alg.Matrix, alg.RowVector) = 
        alg => (m, rv) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((m, rv))

    @targetName("deriveMS2S")
    def derive(
        f: (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Scalar) => alg.Scalar
    ): (alg: MatrixAlgebraDSL) => (alg.Matrix, alg.Scalar) => (alg.Matrix, alg.Scalar) = 
        alg => (m, s) =>
            approxTuple2ScalarWith(alg, 1e-6).derive(f(alg).tupled)((m, s))

    private val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

    private def approxGradient(alg: MatrixAlgebraDSL)(rRes: alg.Scalar, lRes: alg.Scalar, epsilon: alg.Scalar): alg.Scalar = 
        (rRes - lRes) / epsilon

    private def approxScalar(alg: MatrixAlgebraDSL)(s: alg.Scalar, epsilon: alg.Scalar, f: alg.Scalar => alg.Scalar): alg.Scalar =
        val halfEpsilon = epsilon / alg.lift(2.0)
        val r = s + halfEpsilon
        val l = s - halfEpsilon
        approxGradient(alg)(f(r), f(l), epsilon)

    private def approxColumnVector(alg: MatrixAlgebraDSL)(cv: alg.ColumnVector, epsilon: alg.Scalar, f: alg.ColumnVector => alg.Scalar): alg.ColumnVector = 
        val halfEpsilon: alg.Scalar = epsilon / alg.lift(2)
        val res = for (i <- 0 until cv.length)
            yield {
                val r = cv.setElementAt(i, cv.elementAt(i) + halfEpsilon)
                val l = cv.setElementAt(i, cv.elementAt(i) - halfEpsilon)
                approxGradient(alg)(f(r), f(l), epsilon)
            }
        alg.createColumnVectorFromElements(res)

    private def approxRowVector(alg: MatrixAlgebraDSL)(rv: alg.RowVector, epsilon: alg.Scalar, f: alg.RowVector => alg.Scalar): alg.RowVector = 
        val halfEpsilon: alg.Scalar = epsilon / alg.lift(2)
        val res = for (i <- 0 until rv.length)
            yield {
                val r = rv.setElementAt(i, rv.elements(i) + halfEpsilon)
                val l = rv.setElementAt(i, rv.elements(i) - halfEpsilon)
                approxGradient(alg)(f(r), f(l), epsilon)
            }
        alg.createRowVectorFromElements(res)

    private def approxMatrix(alg: MatrixAlgebraDSL)(m: alg.Matrix, epsilon: alg.Scalar, f: alg.Matrix => alg.Scalar): alg.Matrix = 
        val halfEpsilon: alg.Scalar = epsilon / alg.lift(2)
        val res = for (i <- 0 until m.nRows * m.nCols)
            yield {
                val iRow = i / m.nCols
                val jCol = i % m.nCols
                val r = m.setElementAt(iRow, jCol, m.elements(i) + halfEpsilon)
                val l = m.setElementAt(iRow, jCol, m.elements(i) - halfEpsilon)
                approxGradient(alg)(f(r), f(l), epsilon)
            }
        alg.createMatrixFromElements(m.nRows, m.nCols, res)

    def approxScalar2ScalarWith(alg: MatrixAlgebraDSL, epsilon: Double): DeriverFromTo[alg.Scalar => alg.Scalar, alg.Scalar => alg.Scalar] = new DeriverFromTo[alg.Scalar => alg.Scalar, alg.Scalar => alg.Scalar]:
        override def derive(f: alg.Scalar => alg.Scalar): alg.Scalar => alg.Scalar = s =>
            approxScalar(alg)(s, alg.lift(epsilon), f)

    def approxColumnVector2ScalarWith(alg: MatrixAlgebraDSL, epsilon: Double): DeriverFromTo[alg.ColumnVector => alg.Scalar, alg.ColumnVector => alg.ColumnVector] = new DeriverFromTo[alg.ColumnVector => alg.Scalar, alg.ColumnVector => alg.ColumnVector]:
        override def derive(f: alg.ColumnVector => alg.Scalar): alg.ColumnVector => alg.ColumnVector = cv =>
            approxColumnVector(alg)(cv, alg.lift(epsilon), f)

    def approxRowVector2ScalarWith(alg: MatrixAlgebraDSL, epsilon: Double): DeriverFromTo[alg.RowVector => alg.Scalar, alg.RowVector => alg.RowVector] = new DeriverFromTo[alg.RowVector => alg.Scalar, alg.RowVector => alg.RowVector]:
        override def derive(f: alg.RowVector => alg.Scalar): alg.RowVector => alg.RowVector = rv =>
            approxRowVector(alg)(rv, alg.lift(epsilon), f)

    def approxMatrix2ScalarWith(alg: MatrixAlgebraDSL, epsilon: Double): DeriverFromTo[alg.Matrix => alg.Scalar, alg.Matrix => alg.Matrix] = new DeriverFromTo[alg.Matrix => alg.Scalar, alg.Matrix => alg.Matrix]:
        override def derive(f: alg.Matrix => alg.Scalar): alg.Matrix => alg.Matrix = m =>
            approxMatrix(alg)(m, alg.lift(epsilon), f)

    def approxTuple2ScalarWith[T <: Tuple](alg: MatrixAlgebraDSL, epsilon: Double): DeriverFromTo[T => alg.Scalar, T => T] = new DeriverFromTo[T => alg.Scalar, T => T]:

        val epsilonScalar = alg.lift(epsilon)

        override def derive(f: T => alg.Scalar): T => T = t => 
            def numericalPlan(inputs: T): T = 
                val inputsWithIndex = inputs.zip(indices)
                def appendTopAndBottomTo(top: Tuple, x: Any, bottom: Tuple) = 
                    top ++ (x *: bottom)
                inputsWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (s: alg.Scalar, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxScalar(alg)(s, epsilonScalar, s => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, s, bottom).asInstanceOf[T]))
                    case (cv: alg.ColumnVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxColumnVector(alg)(cv, epsilonScalar, cv => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, cv, bottom).asInstanceOf[T]))
                    case (rv: alg.RowVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxRowVector(alg)(rv, epsilonScalar, rv => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, rv, bottom).asInstanceOf[T]))
                    case (m: alg.Matrix, y: Int) =>
                        val (top, bottom) = inputs.splitAt(y)
                        approxMatrix(alg)(m, epsilonScalar, m => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, m, bottom).asInstanceOf[T]))
                }).asInstanceOf[T]
            numericalPlan(t)
