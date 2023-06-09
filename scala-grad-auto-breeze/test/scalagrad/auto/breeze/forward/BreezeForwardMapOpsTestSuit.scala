package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.MapOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import spire.implicits._

import spire.math.Numeric
import spire.algebra.Trig
import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
import DeriverBreezeDoubleForwardPlan.given
import scalagrad.api.forward.dual.DualNumberScalar
import scalagrad.api.fractional.MatrixAlgebraScalarIsFractional.given
import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
import scalagrad.api.spire.trig.DualScalarIsTrig.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardMapOpsTestSuit() extends MapOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        DeriverBreezeDoubleForwardPlan.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)