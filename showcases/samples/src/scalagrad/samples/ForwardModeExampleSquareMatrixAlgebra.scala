package scalagrad

import scalagrad.api.forward.dual.DualNumberScalar


@main
def forwardModeExampleSquareMatrixAlgebra = 
    import scalagrad.api.matrixalgebra.MatrixAlgebra
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

    // define a function
    def square[S](x: S)(using MatrixAlgebra[S, _, _, _]): (S) = 
        x * x

    // import the forwarde mode
    import scalagrad.api.forward.ForwardDualMode
    val mode = ForwardDualMode(BreezeDoubleMatrixAlgebra)
    import mode.given
    
    // derive the function
    val dSquare = mode.derive(square[mode.DualScalar])

    // call the derived function (Note that dSquare(x) = 2 * x)
    assert(dSquare(3.0) == (2 * 3.0)) 
