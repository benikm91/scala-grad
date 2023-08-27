package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait ScalarInvertOps[Scalar]:
    def invert(s: Scalar): Scalar

    extension (s: Scalar)
        @targetName("invert_Op")
        def unary_~ : Scalar = invert(s)