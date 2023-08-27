package scalagrad

import scalagrad.api.dual.DualMatrixAlgebraDSL


@main
def mixtureSample = 
    // import ScalaGrad stuff and the reverse plan
    import scalagrad.api.forward.ForwardMode.derive as dF
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.api.reverse.ReverseMode.derive as dR
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
    
    // import Numeric stuff needed in relu function
    import scalagrad.api.spire.numeric.DualScalarIsNumeric.given
    import spire.implicits.*
    import spire.math.Numeric
    import spire.syntax.numeric.partialOrderOps

    // define relu (relu will be derived by forward plan)
    def relu(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
        val num = summon[Numeric[alg.Scalar]]
        if x < num.zero then num.zero else x

    // define a function using relu (f (except relu) will be derived by reverse plan)
    def f(alg: DualMatrixAlgebraDSL)(
        x: alg.Matrix,
    ): alg.Scalar = 
        import alg.innerAlgebra.*
        val dRelu = dF(relu) // derive relu (using DeriverFromTo)
        val o = x.mapDual(relu(alg.primaryMatrixAlgebra.asDSL), dRelu(alg.primaryMatrixAlgebra.asDSL))
        o.sum

    // derive the function
    val df = dR(f)(BreezeDoubleMatrixAlgebraDSL)
   
    // call the derived function
    val res = df(new breeze.linalg.DenseMatrix(2, 2, Array(-1.0, 2.0, 3.0, 4.0)))
    println(res.toArray.mkString(", "))
    println(res.toArray == Array(0.0, 1.0, 1.0, 1.0))
