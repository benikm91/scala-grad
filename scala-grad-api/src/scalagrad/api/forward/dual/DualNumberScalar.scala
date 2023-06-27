package scalagrad.api.forward.dual

import scalagrad.api.dual.DualScalar

case class DualNumberScalar[S](value: S, derivative: S) extends DualScalar[S, S]:

  inline override def v = value
  inline override def dv = derivative