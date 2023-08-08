package scalagrad

@main
def forwardSample = 
    // import ScalaGrad and the forward plan
    import scalagrad.api.ScalaGrad
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan.given
    import DeriverBreezeDoubleForwardPlan.{algebraT as alg}

    // define a function using the types inside algebraT (algebraT is from forward plan)
    def f(
        x1: alg.Scalar,
        x2: alg.Scalar,
    ): (alg.Scalar, alg.Scalar) = (x2, x1)

    // derive the function
    val df = ScalaGrad.derive(f)

    // call the derived function
    val res: ((Double, Double), (Double, Double)) = df(1.0, 2.0)
    assert(res == ((0.0, 1.0), (1.0, 0.0)))
