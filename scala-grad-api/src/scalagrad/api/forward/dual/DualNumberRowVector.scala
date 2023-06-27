package scalagrad.api.forward.dual

import scalagrad.api.dual.DualRowVector

case class DualNumberRowVector[RV](value: RV, derivative: RV) extends DualRowVector[RV, RV]:

  inline override def v = value
  inline override def dv = derivative