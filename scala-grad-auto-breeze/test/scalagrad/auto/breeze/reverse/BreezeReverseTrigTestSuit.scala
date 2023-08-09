package scalagrad.auto.breeze.reverse.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.TrigTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
import BreezeDoubleReverseMode.given
import spire.implicits._
import scalagrad.api.spire.trig.DualScalarIsTrig.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeReverseTrigTestSuit() extends TrigTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze reverse mode",
        BreezeDoubleReverseMode.algebra,
    ),
    f => ScalaGrad.derive(f)
)