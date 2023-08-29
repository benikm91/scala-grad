# ScalaGrad

ScalaGrad is an automatic differentiation library for Scala which is fast, type-safe, higher-order and can be combined with normal Scala statements.

## TypeSafe

### Minimal Example

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

### General type-strategy with 2D-tensor types

ScalaGrad uses a ```MatrixAlgebra``` type class to abstract over numeric operations, which supports a ```Scalar``` type two ```Vector``` types as well as a ```Matrix``` type.
Note that those represent 0-dimensional, 1-dimensional and 2-dimensional tensors, higher-dimensional tensors are not represented by the ScalaGrad types.

Given a user-defined function $f$, the multiple inputs and multiple outputs can be grouped into ```Vectors``` as well as ```Matrices```.
When deriving $f$ those user-defined logical groups are maintained by ScalaGrad.

```scala mdoc
def identity(alg: MatrixAlgebraDSL)(x: alg.ColumnVector): (alg.ColumnVector) = x

// dIdentity: (DenseVector[Double]) => DenseMatrix[Double]
val dIdentity = d(identity)(BreezeDoubleMatrixAlgebraDSL)

assert(dIdentity(DenseVector[Double](1.0, 2.0)) == DenseMatrix.eye(2))
```
