package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.NumericTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given
import spire.implicits._
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseNumericTestSuit() extends NumericTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        BreezeDoubleReverseMode.algebra,
    ),
    f => ScalaGrad.derive(f)
)