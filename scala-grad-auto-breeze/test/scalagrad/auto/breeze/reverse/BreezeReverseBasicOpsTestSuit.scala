package scalagrad.auto.breeze.Reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.BasicOpsTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.DeriverBreezeReversePlan

import DeriverBreezeReversePlan.tuple2Scalar

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseBasicOpsTestSuit() extends BasicOpsTestSuit(
    "breeze reverse mode",
    DeriverBreezeReversePlan.algebra,
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