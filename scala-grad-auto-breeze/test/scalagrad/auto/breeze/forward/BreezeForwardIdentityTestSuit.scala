package scalagrad.auto.breeze.forward.test

import scalagrad.numerical.DeriverBreezeNumericalPlan
import scalagrad.api.test.IdentityTestSuit
import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.api.DeriverFromTo
import org.scalacheck.Gen

import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
import DeriverBreezeDoubleForwardPlan.given

import breeze.linalg.*
import scalagrad.util.test.BreezeTestUtil

case class BreezeForwardIdentityTestSuit() extends IdentityTestSuit(
    BreezeTestUtil.createGlobalTestSuitParams(
        "breeze forward mode",
        DeriverBreezeDoubleForwardPlan.algebra,
    ),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f),
    f => ScalaGrad.derive(f)
)