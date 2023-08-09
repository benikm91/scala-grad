package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.AccessOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
import BreezeDoubleForwardMode.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardAccessOpsTestSuit() extends AccessOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        BreezeDoubleForwardMode.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)