package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

trait LiftOps[Scalar]:
    def unliftToDouble(s: Scalar): Double
    def liftToScalar(d: Int): Scalar = liftToScalar(d.toDouble)
    def liftToScalar(d: Double): Scalar

    extension (s: Scalar)
        @targetName("unliftToDouble_Ops")
        def toDouble: Double = unliftToDouble(s)