package scalagrad.api

trait Deriver[fT2]:

    final type fT = fT2
    
    type dfT

    def derive(f: fT): (dfT)

object Deriver:

    given [fT, dfT2](using inner: DeriverFromTo[fT, dfT2]): Deriver[fT] with
        type dfT = dfT2
        def derive(f: fT): (dfT2) = inner.derive(f)

trait DeriverFromTo[fT, dfT]:

    def derive(f: fT): (dfT)
