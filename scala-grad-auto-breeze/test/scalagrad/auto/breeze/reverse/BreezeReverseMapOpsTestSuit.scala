package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.MapOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given

import spire.implicits._

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil
import scalagrad.api.fractional.MatrixAlgebraScalarIsFractional.given
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given

case class BreezeReverseMapOpsTestSuit() extends MapOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        BreezeDoubleReverseMode.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)