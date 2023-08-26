package scalagrad.samples

import scalagrad.api.forward.ForwardMode
import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.auto.breeze.BreezeDoubleMatrixAlgebra

def derivePFT(
    f: [S, CV, RV, M] => (alg: MatrixAlgebra[
        S, CV, RV, M,
    ]) => S => S
): [S, CV, RV, M] => (alg: MatrixAlgebra[S, CV, RV, M]) => S => S = 
    [S, CV, RV, M] => (alg: MatrixAlgebra[S, CV, RV, M]) => x =>
        val mode = new ForwardMode(alg)
        mode.tuple2Scalar[Tuple1[mode.DualScalar]].derive((t: Tuple1[mode.DualScalar]) => f(mode.algebra)(t.head))(Tuple1(x)).head
          
def x[S](alg: MatrixAlgebra[S, _, _, _])(s1: S) = 
    import alg.*
    s1 * s1

def x2[S, CV, RV, M](alg: MatrixAlgebra[S, CV, RV, M])(s1: S) = 
    import alg.*
    s1 * s1

@main
def polymorphicFunctionType() =
    val d = derivePFT
    val df2 = d([S, CV, RV, M] => (alg: MatrixAlgebra[S, CV, RV, M]) => x2[S, CV, RV, M](alg))
    // val ddf2 =  d(d(x2))     // May work with https://github.com/lampepfl/dotty/pull/18169
    println(df2(BreezeDoubleMatrixAlgebra)(5.0))

