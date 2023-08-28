package scalagrad


@main
def reverseModeExampleSwap = 
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    // define a function using the types inside algebraT  (algebraT is from reverse plan)
    def swap(alg: MatrixAlgebraDSL)(
        x1: alg.Scalar,
        x2: alg.Scalar,
    ): (alg.Scalar, alg.Scalar) = (x2, x1)

    // import the reverse mode
    import scalagrad.api.reverse.ReverseMode.derive as d

    // derive the function
    val dSwap = d(swap)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function
    assert(dSwap(1.0, 2.0) == ((0.0, 1.0), (1.0, 0.0)))
