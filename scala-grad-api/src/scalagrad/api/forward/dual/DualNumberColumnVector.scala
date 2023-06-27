package scalagrad.api.forward.dual

import scalagrad.api.dual.DualColumnVector

case class DualNumberColumnVector[CV](value: CV, derivative: CV) extends DualColumnVector[CV, CV]:

  inline override def v = value
  inline override def dv = derivative