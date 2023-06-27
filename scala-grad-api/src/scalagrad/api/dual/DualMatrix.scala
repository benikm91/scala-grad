package scalagrad.api.dual

trait DualMatrix[PMatrix, DMatrix]:

  def v: PMatrix
  def dv: DMatrix