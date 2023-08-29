# ScalaGrad

ScalaGrad is an automatic differentiation library for Scala which is fast, type-safe, higher-order and can be combined with normal Scala statements.

## Minimal Example

Let us look at a minimal example $x^2$ to get a feel for ScalaGrad.
First all numerical operations have to be implemented by a ```MatrixAlgebraDSL```,
as we do with the example function ```square```.
Then we can call ```ForwardMode.derive(square)``` to derive the function ```square``` by using forward-mode:

```scala mdoc
@main
def forwardModeExampleSquare = 
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def square(alg: MatrixAlgebraDSL)(x: alg.Scalar): (alg.Scalar) = 
        x * x

    // import the forwarde mode
    import scalagrad.api.forward.ForwardMode.derive as d

    // derive the function
    val dSquare = d(square)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that dSquare(x) = 2 * x)
    assert(dSquare(3.0) == (2 * 3.0)) 
```

If we want to use the reverse-mode, we simply use the function ```ReverseMode.derive``` instead of ```ForwardMode.derive```, the rest stays the same.

```scala mdoc
@main
def reverseModeExampleSquare = 
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL
    
    def square(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
        x * x

    // import the reverse mode
    import scalagrad.api.reverse.ReverseMode.derive as d // <--

    // derive the function
    val dSquare = d(square)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that dSquare(x) = 2 * x)
    assert(dSquare(3.0) == (2 * 3.0)) 

```

## Higher-order

ScalaGrad support higher-order derivatives, by simply deriving the derivative of a function:

```scala mdoc
@main
def higherOrderExampleSquare =
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def square(alg: MatrixAlgebraDSL)(x: alg.Scalar): alg.Scalar = 
        x * x

    // import the forwarde mode
    import scalagrad.api.forward.ForwardMode.derive as d

    // derive the function twice
    val ddSquare = d(d(square))(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function (Note that ddSquare(x) = 2.0)
    assert(ddSquare(5.0) == 2.0)
```

## Fast

ScalaGrads implementation uses memorization for shared parts as well as uses native tensor operations (using Breeze).
ScalaGrad has therefore competetive run-time performance compared to for example PyTorch autograd.
For example the neural network showcase shows a simple neural network architecture classifing the MNIST dataset. One epoch of training (50'000 images) took ScalaGrad 6s compared to 1s with PyTorch.

## Type-safe

ScalaGrad tracks the type when deriving a function, for example the type of the derivative for the ```swap``` function is tracked:

```scala mdoc
@main
def forwardExampleSwap = 
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    def swap(alg: MatrixAlgebraDSL)(
        x1: alg.Scalar,
        x2: alg.Scalar,
    ): (alg.Scalar, alg.Scalar) = (x2, x1)

    import scalagrad.api.forward.ForwardMode.derive as d

    // dSwap: (Double, Double) => ((Double, Double), (Double, Double))
    val dSwap = d(swap)(BreezeDoubleMatrixAlgebraDSL)

    assert(dSwap(1.0, 2.0) == ((0.0, 1.0), (1.0, 0.0)))
```

Type-safety in ScalaGrad is discussed in more details [here](docs/TypeSafe.md)

## ScalaGrad is Scala

ScalaGrad builds the expression graph dynamically (like e.g PyTorch), which means one can use any Scala statement one pleases. If all numerical operations are implemented with the ```MatrixAlgebra``` ScalaGrad simply works.
Here we show how a ```case class``` instance is used inside a function, we want to derive:

```scala mdoc
@main
def complexExampleClass = 
    import scalagrad.api.matrixalgebra.MatrixAlgebra
    import scalagrad.api.matrixalgebra.MatrixAlgebraDSL
    import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebraDSL

    case class Gaussian[S](alg: MatrixAlgebra[S, _, _, _], mu: S, std: S):
        def pdf(x: S): S = 
            import alg.*
            val res = (x - mu) / std
            val normalizationTerm = alg.num.sqrt(alg.lift(2 * Math.PI)) * std
            alg.trig.exp(alg.lift(-0.5) * res * res) / normalizationTerm

    def f(alg: MatrixAlgebraDSL)(mu: alg.Scalar): alg.Scalar = 
        Gaussian(alg.innerAlgebra, mu, alg.lift(2.0)).pdf(alg.lift(1.0))

    // import the reverse mode
    import scalagrad.api.reverse.ReverseMode.derive as d

    // derive the function
    val dF = d(f)(BreezeDoubleMatrixAlgebraDSL)

    // call the derived function
    assert(dF(1.0) == 0.0) 
```