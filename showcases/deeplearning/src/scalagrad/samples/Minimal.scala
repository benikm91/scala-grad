package scalagrad

@main
def minimalExample = 
    // imports
    import scalagrad.api.forward.ForwardMode.derive as d
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    // define a function
    def f(alg: MatrixAlgebraDSL)(x: alg.Scalar): (alg.Scalar) = x * x

    // derive the function
    val df = d(f)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that df(x) = 2 * x)
    assert(df(3.0) == (2 * 3.0)) 
