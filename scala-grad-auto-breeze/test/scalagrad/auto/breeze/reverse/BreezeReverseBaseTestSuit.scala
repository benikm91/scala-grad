package scalagrad.auto.breeze.reverse

import scalagrad.api.test.BaseTestSuit
import scalagrad.api.Mode
import scalagrad.api.reverse.ReverseMode
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

trait BreezeReverseBaseTestSuit extends BaseTestSuit:

    override val deriver: Mode = ReverseMode
    override val pma: MatrixAlgebraDSL = BreezeDoubleMatrixAlgebraDSL
    override val testName: String = "breeze reverse mode"

