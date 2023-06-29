package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.AccessSetOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan
import DeriverBreezeReversePlan.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseAccessSetOpsTestSuit() extends AccessSetOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        DeriverBreezeReversePlan.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)