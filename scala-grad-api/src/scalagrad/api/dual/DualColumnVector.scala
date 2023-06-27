package scalagrad.api.dual

trait DualColumnVector[PCV, DCV]:

  def v: PCV
  def dv: DCV