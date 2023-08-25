package scalagrad


@main
def higherOrderSample =
    import scalagrad.api.ScalaGrad
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.api.forward.ForwardMode
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def f(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = x * x
    
    // Apply ForwardMode.derive twice on function and provide DSL
    val ddf = ForwardMode.derive(ForwardMode.derive(f))(BreezeDoubleMatrixAlgebraDSL)

    assert(ddf(5.0) == 2.0) // ddf(x) = 2.0

    // Apply ForwardMode.derive twice with intermediate and provide DSL
    val df = ForwardMode.derive(f)
    val ddf2 = ForwardMode.derive(df)

    assert(ddf2(BreezeDoubleMatrixAlgebraDSL)(5.0) == 2.0) // ddf(x) = 2.0