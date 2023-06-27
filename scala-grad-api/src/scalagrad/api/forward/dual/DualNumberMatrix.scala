package scalagrad.api.forward.dual

import scalagrad.api.dual.DualMatrix

case class DualNumberMatrix[Matrix](value: Matrix, derivative: Matrix) extends DualMatrix[Matrix, Matrix]:

  inline override def v = value
  inline override def dv = derivative