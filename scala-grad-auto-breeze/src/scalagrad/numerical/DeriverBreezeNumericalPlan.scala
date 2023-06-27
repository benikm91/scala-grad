package scalagrad.numerical

import scalagrad.auto.breeze.BreezeMatrixAlgebra


object DeriverBreezeNumericalPlan extends DeriverNumericalPlan(
    BreezeMatrixAlgebra
)