package scalagrad


@main
def higherOrderExampleSquare =
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def square(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = x * x

    // import the forwarde mode
    import scalagrad.api.forward.ForwardMode.derive as d

    // derive the function twice
    val ddSquare = d(d(square))(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that ddSquare(x) = 2.0)
    assert(ddSquare(5.0) == 2.0)