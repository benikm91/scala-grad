package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait MatrixOps[Matrix, Scalar]:

    def trace(m: Matrix): Scalar 

    def inverse(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("inverse_Op")
        def inv: Matrix = inverse(m)

    def determinant(m: Matrix): Scalar

    extension (m: Matrix)
        @targetName("determinant_Op")
        def det: Scalar = determinant(m)