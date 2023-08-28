package scalagrad

import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

@main
def complexExampleTuple2Tuple = 
    import scalagrad.api.matrixalgebra.MatrixAlgebra
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

    // d has to be specified here for tracking types for SSS => SSS
    def d(
        f: (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => (alg.Scalar, alg.Scalar, alg.Scalar)
    ): (alg: MatrixAlgebraDSL) => (alg.Scalar, alg.Scalar, alg.Scalar) => ((alg.Scalar, alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar, alg.Scalar), (alg.Scalar, alg.Scalar, alg.Scalar)) = 
        alg => (s1, s2, s3) => 
            import scalagrad.api.forward.ForwardDualMode
            val mode = ForwardDualMode(alg.innerAlgebra)
            val df = mode.derive(f(mode.algebraDSL).tupled)
            df(s1, s2, s3)

    def test(alg: MatrixAlgebraDSL)(
        x1: alg.Scalar,
        x2: alg.Scalar,
        x3: alg.Scalar,
    ): (alg.Scalar, alg.Scalar, alg.Scalar) = 
        (x2 * x3, x1 * x3, x1 * x2)

    // import the reverse mode
    import scalagrad.api.reverse.ReverseDualMode

    // derive the function
    val dTest = d(test)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function 
    val (dx1, dx2, dx3) = dTest(1.0, 1.0, 1.0)
    assert(dx1 == (0.0, 1.0, 1.0))
    assert(dx2 == (1.0, 0.0, 1.0))
    assert(dx3 == (1.0, 1.0, 0.0))