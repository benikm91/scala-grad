package scalagrad.api.dual

trait DualScalar[PS, DS]:

  def v: PS
  def dv: DS