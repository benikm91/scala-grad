package scalagrad.api.dual

trait DualRowVector[PRV, DRV]:

  def v: PRV
  def dv: DRV