package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.AccessSetOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseAccessSetOpsTestSuit() extends AccessSetOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        BreezeDoubleReverseMode.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)