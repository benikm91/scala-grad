package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.TrigTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan
import DeriverBreezeForwardPlan.given
import spire.implicits._
import scalagrad.api.spire.trig.DualScalarIsTrig.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardTrigTestSuit() extends TrigTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        DeriverBreezeForwardPlan.algebra,
    ),
    f => ScalaGrad.derive(f),
)