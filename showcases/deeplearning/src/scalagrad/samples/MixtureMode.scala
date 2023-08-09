package scalagrad

@main
def mixtureSample = 
    // import ScalaGrad stuff and the reverse plan
    import scalagrad.api.{ScalaGrad, DeriverFromTo}
    import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode
    import scalagrad.auto.reverse.breeze.BreezeDoubleReverseMode.given
    import BreezeDoubleReverseMode.{algebraT as alg}
    // import Numeric stuff needed in relu function
    import spire.math.Numeric
    import spire.implicits.*
    import scalagrad.api.spire.numeric.DualScalarIsNumeric.given

    // define relu (relu will be derived by forward plan)
    def relu[P: Numeric](x: P): P = 
        val num = summon[Numeric[P]]
        if num.lt(x, num.zero) then num.zero else x

    // define a function using relu (f (except relu) will be derived by reverse plan)
    def f(using DeriverFromTo[alg.Scalar => alg.Scalar, alg.PrimaryScalar => alg.PrimaryScalar])(
        x: alg.Matrix,
    ): alg.Scalar = 
        import alg.innerAlgebra.*
        val dRelu = ScalaGrad.derive(relu[alg.Scalar]) // derive relu (using DeriverFromTo)
        val o = x.mapDual(relu[alg.PrimaryScalar], dRelu)
        o.sum

    // import the forward plan which will be given to f.
    import scalagrad.auto.forward.breeze.BreezeDoubleForwardMode.given
    
    // derive the function
    val df = ScalaGrad.derive(f)
   
    // call the derived function
    val res = df(new breeze.linalg.DenseMatrix(2, 2, Array(-1.0, 2.0, 3.0, 4.0)))
    println(res.toArray == Array(0.0, 1.0, 1.0, 1.0))
