[![codecov](https://codecov.io/gh/benikm91/scala-grad/branch/main/graph/badge.svg?token=X7LE1VFFTC)](https://codecov.io/gh/benikm91/scala-grad)

# ScalaGrad

ScalaGrad is an automatic differentiation library for Scala.
It is fast, type-safe, higher-order and can be combined with normal Scala statements. 
It is designed to be flexibel and can be used to different applications (see [Showcases](#showcases)).

## State of the project

ScalaGrad got developed during my master thesis which ended on 31. september 2023.
ScalaGrad is in a usable state and got applied successfully to a wide range of showcases.
It will be maintained in my free time, therefore bugfixes and further development are not a given.

## QuickStart

An introduction to ScalaGrads features as well as minimal examples are given in the [QuickStart](docs/QuickStart.md).

## Examples and Showcases

ScalaGrad supports the forward-mode, reverse-mode a manual version of a mixture-mode, higher-order derivatives and can be applied to more complex applications. 

In this repository we have implemented simple [examples](showcases/samples/src/scalagrad/samples/) showing how those different features can be used.
As well as more complex applications:
- Train a neural network on MNIST (see [Code](showcases/machinelearning/src/scalagrad/showcase/deeplearning/mnist/NeuralNetworkMNIST.scala))
- Train a linear regression (see [Code](showcases/machinelearning/src/scalagrad/showcase/linearregression/LinearRegression.scala))
- Use higher-order gradients for the newton method (see [Code](showcases/machinelearning/src/scalagrad/showcase/linearregression/newtonmethod/NewtonMethod.scala))
- Sample a bayesian linear model with the Metropolis-adjusted Langevin algorithm (MALA) sampler as well as the Hamiltonian Monte Carlo sampler (see [Code](showcases/probabilisticprogramming/src/scalagrad/showcase/probabilisticprogramming/UseCase1b.scala))

## Extra

Original master thesis: [ScalaGrad - Automatic Differentiation for Scala](https://github.com/benikm91/scala-grad-thesis/blob/main/thesis-v1.pdf)

Derived publication: [ScalaGrad: A Statically Typed Automatic
Differentiation Library for Safer Data Science](https://digitalcollection.zhaw.ch/server/api/core/bitstreams/3d260a58-e3b0-4555-8750-875176e48ec5/content)

