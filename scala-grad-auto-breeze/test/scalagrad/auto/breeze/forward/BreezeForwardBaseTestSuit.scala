package scalagrad.auto.breeze.forward

import scalagrad.api.test.BaseTestSuit
import scalagrad.api.ModeO
import scalagrad.api.forward.ForwardMode
import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

trait BreezeForwardBaseTestSuit extends BaseTestSuit:

    override val deriver: ModeO = ForwardMode
    override val pma: MatrixAlgebraDSL = BreezeDoubleMatrixAlgebraDSL
    override val testName: String = "breeze forward mode"

