package scalagrad

@main
def higherOrderSample =
    import scalagrad.api.ScalaGrad
    import scalagrad.api.forward.DeriverForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import DeriverBreezeDoubleForwardPlan.{algebraGiven => _, given}

    // construct DeriverPlan for deriving twice by chaining plan
    val ffp = new DeriverForwardPlan(DeriverBreezeDoubleForwardPlan.algebraGiven)
    import ffp.given

    def f(x: ffp.algebraT.Scalar): ffp.algebraT.Scalar = x * x
    
    // Apply ScalaGrad.derive twice on function (here twice forward mode)
    val ddf = ScalaGrad.derive(ScalaGrad.derive(f))

    assert(ddf(5.0) == 2.0) // ddf(x) = 2.0