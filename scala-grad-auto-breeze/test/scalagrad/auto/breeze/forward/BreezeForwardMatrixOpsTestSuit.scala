package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.MatrixOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import spire.implicits._

import spire.math.Numeric
import spire.algebra.Trig
import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
import BreezeDoubleForwardMode.given
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.fractional.MatrixAlgebraScalarIsFractional.given
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardMatrixOpsTestSuit() extends MatrixOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        BreezeDoubleForwardMode.algebra,
    ),
    f => ScalaGrad.derive(f),
)