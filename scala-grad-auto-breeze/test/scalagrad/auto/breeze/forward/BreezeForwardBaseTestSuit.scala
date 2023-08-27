package scalagrad.auto.breeze.forward

import scalagrad.api.Mode
import scalagrad.api.forward.ForwardMode
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.api.test.BaseTestSuit
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

trait BreezeForwardBaseTestSuit extends BaseTestSuit:

    override val deriver: Mode = ForwardMode
    override val pma: MatrixAlgebraDSL = BreezeDoubleMatrixAlgebraDSL
    override val testName: String = "breeze forward mode"

