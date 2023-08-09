package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.AccessSetOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
import BreezeDoubleForwardMode.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardAccessSetOpsTestSuit() extends AccessSetOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        BreezeDoubleForwardMode.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)