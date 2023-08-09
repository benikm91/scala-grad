package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.NumericTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
import BreezeDoubleForwardMode.given
import spire.implicits._
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardNumericTestSuit() extends NumericTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        BreezeDoubleForwardMode.algebra,
    ),
    f => ScalaGrad.derive(f),
)