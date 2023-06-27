package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.BasicOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.DeriverBreezeForwardPlan

import DeriverBreezeForwardPlan.tuple2Scalar

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardBasicOpsTestSuit() extends BasicOpsTestSuit(
    "breeze forward mode",
    DeriverBreezeForwardPlan.algebra,
    DeriverBreezeNumericalPlan.algebra,
    BreezeTestUtil.reasonableDenseMatrixDoubleGenerator,
    BreezeTestUtil.reasonableDenseVectorDoubleGenerator,
    (x: Gen[Int]) => for { cv <- BreezeTestUtil.reasonableDenseVectorDoubleGenerator(x) } yield cv.t,
    BreezeTestUtil.reasonableDoubleGenerator,
    DeriverBreezeNumericalPlan,
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)