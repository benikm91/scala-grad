package scalagrad


@main
def higherOrderSample =
    import scalagrad.api.forward.ForwardMode.derive as d
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def f(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = x * x
    
    // Apply ForwardMode.derive twice on function and provide DSL
    val ddf = d(d(f))(BreezeDoubleMatrixAlgebraDSL)

    assert(ddf(5.0) == 2.0) // ddf(x) = 2.0

    // Apply ForwardMode.derive twice with intermediate and provide DSL
    val df = d(f)
    val ddf2 = d(df)

    assert(ddf2(BreezeDoubleMatrixAlgebraDSL)(5.0) == 2.0) // ddf(x) = 2.0