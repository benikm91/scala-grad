package scalagrad.api

object ScalaGrad:

    def derive[S, T](f: S => T)(using d: Deriver[S => T]): d.dfT = 
        d.derive(f)

    def derive[S1, S2, T](f: (S1, S2) => T)(using d: Deriver[((S1, S2)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, T](f: (S1, S2, S3) => T)(using d: Deriver[((S1, S2, S3)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, T](f: (S1, S2, S3, S4) => T)(using d: Deriver[((S1, S2, S3, S4)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, S5, T](f: (S1, S2, S3, S4, S5) => T)(using d: Deriver[((S1, S2, S3, S4, S5)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, S5, S6, T](f: (S1, S2, S3, S4, S5, S6) => T)(using d: Deriver[((S1, S2, S3, S4, S5, S6)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, S5, S6, S7, T](f: (S1, S2, S3, S4, S5, S6, S7) => T)(using d: Deriver[((S1, S2, S3, S4, S5, S6, S7)) => T]): d.dfT = 
        derive(f.tupled)

    def derive[S1, S2, S3, S4, S5, S6, S7, S8, T](f: (S1, S2, S3, S4, S5, S6, S7, S8) => T)(using d: Deriver[((S1, S2, S3, S4, S5, S6, S7, S8)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, S5, S6, S7, S8, S9, T](f: (S1, S2, S3, S4, S5, S6, S7, S8, S9) => T)(using d: Deriver[((S1, S2, S3, S4, S5, S6, S7, S8, S9)) => T]): d.dfT = 
        derive(f.tupled)
        
    def derive[S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, T](f: (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10) => T)(using d: Deriver[((S1, S2, S3, S4, S5, S6, S7, S8, S9, S10)) => T]): d.dfT = 
        derive(f.tupled)
        