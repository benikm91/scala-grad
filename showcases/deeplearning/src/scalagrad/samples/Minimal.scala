package scalagrad

@main
def minimalExample = 
    // imports
    import scalagrad.api.ScalaGrad
    import scalagrad.api.matrixalgebra.MatrixAlgebraT
    import scalagrad.auto.forward.breeze.DeriverBreezeDoubleForwardPlan
    import DeriverBreezeDoubleForwardPlan.given

    // define a function
    def f(alg: MatrixAlgebraT)(x: alg.Scalar): (alg.Scalar) = x * x

    // derive the function
    val df = ScalaGrad.derive(f(DeriverBreezeDoubleForwardPlan.algebraT))

    // call the derived function (Note that df(x) = 2 * x)
    assert(df(3.0) == (2 * 3.0)) 
