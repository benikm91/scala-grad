package scalagrad


@main
def reverseModeExampleSquare = 
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
    
    // define a function
    def square(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = x * x

    // import the reverse mode
    import scalagrad.api.reverse.ReverseMode.derive as d

    // derive the function
    val dSquare = d(square)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that dSquare(x) = 2 * x)
    assert(dSquare(3.0) == (2 * 3.0)) 
