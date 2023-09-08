package scalagrad

import scalagrad.auto.forward.BreezeDoubleForwardDualMode.derive as d
import scalagrad.auto.forward.BreezeDoubleForwardDualMode.algebra.*  // import syntax (extension method) 
import scalagrad.auto.forward.BreezeDoubleForwardDualMode.algebraDSL as alg

/**
  * In this example we show how a hard coded dual algebra can be used. 
  * This may be a fallback solution. It is less flexible (fixed mode and breeze), but easier to use (no MatrixAlgebra passing).
  */

@main
def hardcodeModeExample = 
    def square(x: alg.Scalar): (alg.Scalar) = 
        x * x

    val dSquare = d(square)

    assert(dSquare(3.0) == (2 * 3.0)) 