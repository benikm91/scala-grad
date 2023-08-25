package scalagrad

@main
def minimalExample = 
    // imports
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
    import scalagrad.api.forward.ForwardMode

    // define a function
    def f(alg: MatrixAlgebraDSL)(x: alg.Scalar): (alg.Scalar) = x * x

    // derive the function
    val df = ForwardMode.derive(f)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that df(x) = 2 * x)
    assert(df(3.0) == (2 * 3.0)) 
