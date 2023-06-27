package scalagrad.numerical

import scalagrad.api.DeriverFromTo
import scalagrad.api.matrixalgebra.MatrixAlgebra
import breeze.linalg.{DenseVector, Transpose, DenseMatrix}

abstract class DeriverNumericalPlan[
    // TODO currently type bounds are needed to make the compiler happy, but they should not be needed
    PScalar <: Double, PColumnVector <: DenseVector[Double], PRowVector <: Transpose[DenseVector[Double]], PMatrix <: DenseMatrix[Double],
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

    given approx[T <: Tuple : PTuple]: DeriverFromTo[T => PScalar, T => T] = approxWith(a.liftToScalar(1e-6))

    def approxWith[T <: Tuple : PTuple](epsilon: PScalar): DeriverFromTo[T => PScalar, T => T] = new DeriverFromTo[T => PScalar, T => T]:

        override def derive(f: T => PScalar): T => T = t => 
            def numericalPlan(inputs: T): T = 
                val halfEpsilon: PScalar = a.divideSS(epsilon, a.liftToScalar(2.0))
                val inputsWithIndex = inputs.zip(indices)
                def appendTopAndBottomTo(top: Tuple, x: Any, bottom: Tuple) = 
                    top ++ (x *: bottom)
                inputsWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                    case (x: PScalar, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        val r = a.plusSS(x, halfEpsilon)
                        val l = a.minusSS(x, halfEpsilon)
                        a.divideSS(
                            a.minusSS(
                                f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, r, bottom).asInstanceOf[T]),
                                f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, l, bottom).asInstanceOf[T]),
                            ),
                            epsilon
                        )
                    case (x: PColumnVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        val res = for (i <- 0 until x.length)
                            yield {
                                val r = createColumnVectorFromElements(x.elements.toVector.updated(i, a.plusSS(x.elements(i), halfEpsilon)))
                                val l = createColumnVectorFromElements(x.elements.toVector.updated(i, a.minusSS(x.elements(i), halfEpsilon)))
                                a.divideSS(
                                    a.minusSS(
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, r, bottom).asInstanceOf[T]),
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, l, bottom).asInstanceOf[T]),
                                    ),
                                    epsilon
                                )
                            }
                        createColumnVectorFromElements(res)
                    case (x: PRowVector, y: Int) => 
                        val (top, bottom) = inputs.splitAt(y)
                        val res = for (i <- 0 until x.length)
                            yield {
                                val r = createRowVectorFromElements(x.elements.toVector.updated(i, a.plusSS(x.elements(i), halfEpsilon)))
                                val l = createRowVectorFromElements(x.elements.toVector.updated(i, a.minusSS(x.elements(i), halfEpsilon)))
                                a.divideSS(
                                    a.minusSS(
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, r, bottom).asInstanceOf[T]),
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, l, bottom).asInstanceOf[T]),
                                    ),
                                    epsilon
                                )
                            }
                        createRowVectorFromElements(res)
                    case (x: PMatrix, y: Int) =>
                        val (top, bottom) = inputs.splitAt(y)
                        val res = for (i <- 0 until x.nRows * x.nCols)
                            yield {
                                val r = createMatrixFromElements(x.nRows, x.nCols, x.elements.toVector.updated(i, a.plusSS(x.elements(i), halfEpsilon)))
                                val l = createMatrixFromElements(x.nRows, x.nCols, x.elements.toVector.updated(i, a.minusSS(x.elements(i), halfEpsilon)))
                                a.divideSS(
                                    a.minusSS(
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, r, bottom).asInstanceOf[T]),
                                        f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, l, bottom).asInstanceOf[T]),
                                    ),
                                    epsilon
                                )
                            }
                        createMatrixFromElements(x.nRows, x.nCols, res)
                        // createMatrixFromElements(x.nCols, x.nRows, res).t
                }).asInstanceOf[T]
            numericalPlan(t)
