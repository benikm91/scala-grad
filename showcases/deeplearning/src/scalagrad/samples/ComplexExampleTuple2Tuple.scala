package scalagrad


@main
def complexExampleTuple2Tuple = 
    import scalagrad.api.matrixalgebra.MatrixAlgebra
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

    def test(alg: MatrixAlgebraDSL)(
        x1: alg.Scalar,
        x2: alg.Scalar,
        x3: alg.Scalar,
    ): (alg.Scalar, alg.Scalar, alg.Scalar) = 
        (x2 * x3, x1 * x3, x1 * x2)

    // import the reverse mode
    import scalagrad.api.reverse.ReverseDualMode

    // derive the function
    val dualMode = ReverseDualMode(BreezeDoubleMatrixAlgebra)
    val dTest = dualMode.deriveDualTuple2DualTuple(test(dualMode.algebraDSL).tupled)

    // call the derived function 
    val (dx1, dx2, dx3) = dTest(1.0, 1.0, 1.0)
    assert(dx1 == (0.0, 1.0, 1.0))
    assert(dx2 == (1.0, 0.0, 1.0))
    assert(dx3 == (1.0, 1.0, 0.0))