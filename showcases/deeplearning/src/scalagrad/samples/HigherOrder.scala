package scalagrad

@main
def higherOrderSample =
    import scalagrad.api.ScalaGrad
    import scalagrad.api.forward.ForwardMode
    import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode
    import BreezeDoubleForwardMode.{algebraGiven => _, given}

    // stack modes for deriving twice
    val ffp = new ForwardMode(BreezeDoubleForwardMode.algebra)
    import ffp.given

    def f(x: ffp.algebraT.Scalar): ffp.algebraT.Scalar = x * x
    
    // Apply ScalaGrad.derive twice on function (here twice forward mode)
    val ddf = ScalaGrad.derive(ScalaGrad.derive(f))

    assert(ddf(5.0) == 2.0) // ddf(x) = 2.0