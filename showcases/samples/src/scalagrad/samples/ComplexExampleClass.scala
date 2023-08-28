package scalagrad


@main
def complexExampleClass = 
    import scalagrad.api.matrixalgebra.MatrixAlgebra
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    // MatrixAlgebraDSL can't be used successfully in classes => use type class MatrixAlgebra instead
    case class Gaussian[S](alg: MatrixAlgebra[S, _, _, _], mu: S, std: S):
        def pdf(x: S): S = 
            import alg.*
            val res = (x - mu) / std
            val normalizationTerm = alg.num.sqrt(alg.lift(2 * Math.PI)) * std
            alg.trig.exp(alg.lift(-0.5) * res * res) / normalizationTerm

    def test(alg: MatrixAlgebraDSL)(mu: alg.Scalar): alg.Scalar = 
        Gaussian(alg.innerAlgebra, mu, alg.lift(2.0)).pdf(alg.lift(1.0))

    // import the reverse mode
    import scalagrad.api.reverse.ReverseMode.derive as d

    // derive the function
    val dTest = d(test)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that dTest(x) = 2 * x)
    assert(dTest(1.0) == 0.0) 