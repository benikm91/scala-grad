package scalagrad.numerical

import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra


object DeriverBreezeNumericalPlan extends DeriverNumericalPlan(
    BreezeDoubleMatrixAlgebra
)