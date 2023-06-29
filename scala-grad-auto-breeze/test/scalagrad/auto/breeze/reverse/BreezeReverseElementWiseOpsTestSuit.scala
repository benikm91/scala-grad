package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.ElementWiseOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan
import DeriverBreezeReversePlan.given

import spire.implicits._

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil
import scalagrad.api.fractional.MatrixAlgebraScalarIsFractional.given
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given

case class BreezeReverseElementWiseOpsTestSuit() extends ElementWiseOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        DeriverBreezeReversePlan.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)