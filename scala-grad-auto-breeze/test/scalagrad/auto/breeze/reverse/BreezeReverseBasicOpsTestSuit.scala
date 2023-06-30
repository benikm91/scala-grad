package scalagrad.auto.breeze.Reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.BasicOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan

import DeriverBreezeDoubleReversePlan.tuple2Scalar

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseBasicOpsTestSuit() extends BasicOpsTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        DeriverBreezeDoubleReversePlan.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)