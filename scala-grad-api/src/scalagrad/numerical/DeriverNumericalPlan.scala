package scalagrad.numerical

import scalagrad.api.DeriverFromTo
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scala.reflect.Typeable

abstract class DeriverNumericalPlan[
    PScalar : Typeable, PColumnVector : Typeable, PRowVector : Typeable, PMatrix : Typeable,
](
    val algebra: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix],
):

    type PTuple[T <: Tuple] = T match
        case PScalar *: t => PTuple[t]
        case PColumnVector *: t => PTuple[t]
        case PRowVector *: t => PTuple[t]
        case PMatrix *: t => PTuple[t]
        case EmptyTuple => DummyImplicit

    private val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

    import algebra.*
    val a = algebra

    private def approxGradient(rRes: PScalar, lRes: PScalar, epsilon: PScalar): PScalar = 
        a.divideSS(
            a.minusSS(
                rRes,
                lRes,
            ),
            epsilon
        )

    private def approxScalar(s: PScalar, epsilon: PScalar, f: PScalar => PScalar): PScalar =
        val halfEpsilon: PScalar = a.divideSS(epsilon, a.liftToScalar(2.0))
        val r = a.plusSS(s, halfEpsilon)
        val l = a.minusSS(s, halfEpsilon)
        approxGradient(f(r), f(l), epsilon)

    private def approxColumnVector(cv: PColumnVector, epsilon: PScalar, f: PColumnVector => PScalar): PColumnVector = 
        val halfEpsilon: PScalar = a.divideSS(epsilon, a.liftToScalar(2.0))
        val res = for (i <- 0 until cv.length)
            yield {
                val r = createColumnVectorFromElements(cv.elements.toVector.updated(i, a.plusSS(cv.elements(i), halfEpsilon)))
                val l = createColumnVectorFromElements(cv.elements.toVector.updated(i, a.minusSS(cv.elements(i), halfEpsilon)))
                approxGradient(
                    f(r),
                    f(l),
                    epsilon
                )
            }
        a.createColumnVectorFromElements(res)

    private def approxRowVector(rv: PRowVector, epsilon: PScalar, f: PRowVector => PScalar): PRowVector = 
        val halfEpsilon: PScalar = a.divideSS(epsilon, a.liftToScalar(2.0))
        val res = for (i <- 0 until rv.length)
            yield {
                val r = createRowVectorFromElements(rv.elements.toVector.updated(i, a.plusSS(rv.elements(i), halfEpsilon)))
                val l = createRowVectorFromElements(rv.elements.toVector.updated(i, a.minusSS(rv.elements(i), halfEpsilon)))
                approxGradient(f(r), f(l), epsilon)
            }
        a.createRowVectorFromElements(res)

    private def approxMatrix(m: PMatrix, epsilon: PScalar, f: PMatrix => PScalar): PMatrix = 
        val halfEpsilon: PScalar = a.divideSS(epsilon, a.liftToScalar(2.0))
        val res = for (i <- 0 until m.nRows * m.nCols)
            yield {
                val r = createMatrixFromElements(m.nRows, m.nCols, m.elements.toVector.updated(i, a.plusSS(m.elements(i), halfEpsilon)))
                val l = createMatrixFromElements(m.nRows, m.nCols, m.elements.toVector.updated(i, a.minusSS(m.elements(i), halfEpsilon)))
                approxGradient(f(r), f(l), epsilon)
            }
        createMatrixFromElements(m.nRows, m.nCols, res)

    given approxTuple2ScalarDefault[T <: Tuple : PTuple]: DeriverFromTo[T => PScalar, T => T] = approxTuple2ScalarWith(1e-6)
    given approxScalar2ScalarDefault: DeriverFromTo[PScalar => PScalar, PScalar => PScalar] = approxScalar2ScalarWith(1e-6)
    given approxColumnVector2ScalarDefault: DeriverFromTo[PColumnVector => PScalar, PColumnVector => PColumnVector] = approxColumnVector2ScalarWith(1e-6)
    given approxRowVector2ScalarDefault: DeriverFromTo[PRowVector => PScalar, PRowVector => PRowVector] = approxRowVector2ScalarWith(1e-6)
    given approxMatrix2ScalarDefault: DeriverFromTo[PMatrix => PScalar, PMatrix => PMatrix] = approxMatrix2ScalarWith(1e-6)
    
    def approxScalar2ScalarWith(epsilon: Double): DeriverFromTo[PScalar => PScalar, PScalar => PScalar] = new DeriverFromTo[PScalar => PScalar, PScalar => PScalar]:
        override def derive(f: PScalar => PScalar): PScalar => PScalar = s =>
            approxScalar(s, a.liftToScalar(epsilon), f)

    def approxColumnVector2ScalarWith(epsilon: Double): DeriverFromTo[PColumnVector => PScalar, PColumnVector => PColumnVector] = new DeriverFromTo[PColumnVector => PScalar, PColumnVector => PColumnVector]:
        override def derive(f: PColumnVector => PScalar): PColumnVector => PColumnVector = cv =>
            approxColumnVector(cv, a.liftToScalar(epsilon), f)

    def approxRowVector2ScalarWith(epsilon: Double): DeriverFromTo[PRowVector => PScalar, PRowVector => PRowVector] = new DeriverFromTo[PRowVector => PScalar, PRowVector => PRowVector]:
        override def derive(f: PRowVector => PScalar): PRowVector => PRowVector = rv =>
            approxRowVector(rv, a.liftToScalar(epsilon), f)

    def approxMatrix2ScalarWith(epsilon: Double): DeriverFromTo[PMatrix => PScalar, PMatrix => PMatrix] = new DeriverFromTo[PMatrix => PScalar, PMatrix => PMatrix]:
        override def derive(f: PMatrix => PScalar): PMatrix => PMatrix = m =>
            approxMatrix(m, a.liftToScalar(epsilon), f)

    def approxTuple2ScalarWith[T <: Tuple : PTuple](epsilon: Double): DeriverFromTo[T => PScalar, T => T] = new DeriverFromTo[T => PScalar, T => T]:

        val epsilonScalar = a.liftToScalar(epsilon)

        override def derive(f: T => PScalar): T => T = t => 
            def numericalPlan(inputs: T): T = 
                val inputsWithIndex = inputs.zip(indices)
                def appendTopAndBottomTo(top: Tuple, x: Any, bottom: Tuple) = 
                    top ++ (x *: bottom)
                inputsWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (s: PScalar, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxScalar(s, epsilonScalar, s => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, s, bottom).asInstanceOf[T]))
                    case (cv: PColumnVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxColumnVector(cv, epsilonScalar, cv => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, cv, bottom).asInstanceOf[T]))
                    case (rv: PRowVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        approxRowVector(rv, epsilonScalar, rv => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, rv, bottom).asInstanceOf[T]))
                    case (m: PMatrix, y: Int) =>
                        val (top, bottom) = inputs.splitAt(y)
                        approxMatrix(m, epsilonScalar, m => f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, m, bottom).asInstanceOf[T]))
                }).asInstanceOf[T]
            numericalPlan(t)
