package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.NumericTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan
import DeriverBreezeForwardPlan.given
import spire.implicits._
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardNumericTestSuit() extends NumericTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        DeriverBreezeForwardPlan.algebra,
    ),
    f => ScalaGrad.derive(f),
)