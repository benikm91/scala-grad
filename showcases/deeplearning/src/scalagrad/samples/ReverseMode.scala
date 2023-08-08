package scalagrad

@main
def reverseSample = 
    // import ScalaGrad and the reverse plan
    import scalagrad.api.ScalaGrad
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan
    import scalagrad.auto.reverse.breeze.DeriverBreezeDoubleReversePlan.given
    import DeriverBreezeDoubleReversePlan.{algebraT as alg}

    // define a function using the types inside algebraT  (algebraT is from reverse plan)
    def f(
        x1: alg.Scalar,
        x2: alg.Scalar,
    ): (alg.Scalar, alg.Scalar) = (x2, x1)

    // derive the function
    val df = ScalaGrad.derive(f)

    // call the derived function
    val res = df(1.0, 2.0)
    assert(res == ((0.0, 1.0), (1.0, 0.0)))