[![codecov](https://codecov.io/gh/benikm91/scala-grad/branch/main/graph/badge.svg?token=X7LE1VFFTC)](https://codecov.io/gh/benikm91/scala-grad)

# ScalaGrad

ScalaGrad is an automatic differentiation library for Scala.
It is fast, type-safe, higher-order and can be combined with normal Scala statements. It is designed to be flexibel and can be used to different applications (see [Showcases](#showcases)).

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