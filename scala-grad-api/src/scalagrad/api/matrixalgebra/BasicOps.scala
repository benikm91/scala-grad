package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait BasicOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: NegateOps[Scalar, ColumnVector, RowVector, Matrix] 
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with ScalarInvertOps[Scalar] =>

    def dotMM(m1: Matrix, m2: Matrix): Matrix
    def dotMCV(m: Matrix, cv: ColumnVector): ColumnVector
    
    extension (m: Matrix)
        @targetName("dotMM_Op")
        def *(m2: Matrix): Matrix = dotMM(m, m2)
        @targetName("dotMCV_Op")
        def *(cv: ColumnVector): ColumnVector = dotMCV(m, cv)
        
    def dotCVRV(cv: ColumnVector, rv: RowVector): Matrix

    extension (cv: ColumnVector)
        @targetName("dotCVRV_Op")
        def *(rv: RowVector): Matrix = dotCVRV(cv, rv)

    def dotRVM(rv: RowVector, m: Matrix): RowVector = (m.t * rv.t).t
    def dotRVCV(rv: RowVector, cv: ColumnVector): Scalar

    extension (rv: RowVector)
        @targetName("dotRVM_Op")
        def *(m: Matrix): RowVector = dotRVM(rv, m)
        @targetName("dotRVCV_Op")
        def *(cv: ColumnVector): Scalar = dotRVCV(rv, cv)

    def plusMM(m1: Matrix, m2: Matrix): Matrix
    def plusMCV(m: Matrix, cv: ColumnVector): Matrix
    def plusMRV(m: Matrix, rv: RowVector): Matrix = (m.t + rv.t).t
    def plusMS(m: Matrix, s: Scalar): Matrix

    extension (m: Matrix)
        @targetName("plusMM_Op")
        def +(m2: Matrix): Matrix = plusMM(m, m2)
        @targetName("plusMCV_Op")
        def +(cv: ColumnVector): Matrix = plusMCV(m, cv)
        @targetName("plusMRV_Op")
        def +(rv: RowVector): Matrix = plusMRV(m, rv)
        @targetName("plusMS_Op")
        def +(s: Scalar): Matrix = plusMS(m, s)
        
    def plusCVM(cv: ColumnVector, m: Matrix): Matrix = plusMCV(m, cv)
    def plusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector
    def plusCVS(cv: ColumnVector, s: Scalar): ColumnVector

    extension (cv: ColumnVector)
        @targetName("plusCVM_Op")
        def +(m: Matrix): Matrix = plusCVM(cv, m)
        @targetName("plusCVCV_Op")
        def +(cv2: ColumnVector): ColumnVector = plusCVCV(cv, cv2)
        @targetName("plusCVS_Op")
        def +(s: Scalar): ColumnVector = plusCVS(cv, s)

    def plusRVM(rv: RowVector, m: Matrix): Matrix = plusMRV(m, rv)
    def plusRVRV(rv1: RowVector, rv2: RowVector): RowVector = (rv1.t + rv2.t).t
    def plusRVS(rv: RowVector, s: Scalar): RowVector = (rv.t + s).t
        
    extension (rv: RowVector)
        @targetName("plusRVM_Op")
        def +(m: Matrix): Matrix = plusRVM(rv, m)
        @targetName("plusRVRV_Op")
        def +(rv2: RowVector): RowVector = plusRVRV(rv, rv2)
        @targetName("plusRVS_Op")
        def +(s: Scalar): RowVector = plusRVS(rv, s)

    def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    def plusSCV(s: Scalar, cv: ColumnVector): ColumnVector = plusCVS(cv, s)
    def plusSRV(s: Scalar, rv: RowVector): RowVector = plusRVS(rv, s)
    def plusSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("plusSM_Op")
        def +(m: Matrix): Matrix = plusSM(s, m)
        @targetName("plusSCV_Op")
        def +(cv: ColumnVector): ColumnVector = plusSCV(s, cv)
        @targetName("plusSRV_Op")
        def +(rv: RowVector): RowVector = plusSRV(s, rv)
        @targetName("plusSS_Op")
        def +(s2: Scalar): Scalar = plusSS(s, s2)

    def minusMM(m1: Matrix, m2: Matrix): Matrix = plusMM(m1, negateM(m2))
    def minusMCV(m: Matrix, cv: ColumnVector): Matrix = plusMCV(m, negateCV(cv))
    def minusMRV(m: Matrix, rv: RowVector): Matrix = plusMRV(m, negateRV(rv))
    def minusMS(m: Matrix, s: Scalar): Matrix = plusMS(m, negateS(s))

    extension (m1: Matrix)
        @targetName("minusMM_Op")
        def -(m2: Matrix): Matrix = minusMM(m1, m2)
        @targetName("minusMCV_Op")
        def -(cv: ColumnVector): Matrix = minusMCV(m1, cv)
        @targetName("minusMRV_Op")
        def -(rv: RowVector): Matrix = minusMRV(m1, rv)
        @targetName("minusMS_Op")
        def -(s: Scalar): Matrix = minusMS(m1, s)

    def minusCVM(cv: ColumnVector, m: Matrix): Matrix = minusMCV(m, cv)
    def minusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector = plusCVCV(cv1, negateCV(cv2))
    def minusCVS(cv: ColumnVector, s: Scalar): ColumnVector = plusCVS(cv, negateS(s))
    
    extension (cv: ColumnVector)
        @targetName("minusCVM_Op")
        def -(m: Matrix): Matrix = minusCVM(cv, m)
        @targetName("minusCVCV_Op")
        def -(cv2: ColumnVector): ColumnVector = minusCVCV(cv, cv2)
        @targetName("minusCVS_Op")
        def -(s: Scalar): ColumnVector = minusCVS(cv, s)

    def minusRVM(rv: RowVector, m: Matrix): Matrix = minusMRV(m, rv)
    def minusRVRV(rv1: RowVector, rv2: RowVector): RowVector = plusRVRV(rv1, negateRV(rv2))
    def minusRVS(rv: RowVector, s: Scalar): RowVector = plusRVS(rv, negateS(s))

    extension (rv: RowVector)
        @targetName("minusRVM_Op")
        def -(m: Matrix): Matrix = minusRVM(rv, m)
        @targetName("minusRVRV_Op")
        def -(rv2: RowVector): RowVector = minusRVRV(rv, rv2)
        @targetName("minusRVS_Op")
        def -(s: Scalar): RowVector = minusRVS(rv, s)

    def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    def minusSCV(s: Scalar, cv: ColumnVector): ColumnVector = minusCVS(cv, s)
    def minusSRV(s: Scalar, rv: RowVector): RowVector = minusRVS(rv, s)
    def minusSS(s1: Scalar, s2: Scalar): Scalar = plusSS(s1, negateS(s2))

    extension (s: Scalar)
        @targetName("minusSM_Op")
        def -(m: Matrix): Matrix = minusSM(s, m)
        @targetName("minusSCV_Op")
        def -(cv: ColumnVector): ColumnVector = minusSCV(s, cv)
        @targetName("minusSRV_Op")
        def -(rv: RowVector): RowVector = minusSRV(s, rv)
        @targetName("minusSS_Op")
        def -(s2: Scalar): Scalar = minusSS(s, s2)

    def divideMS(m: Matrix, s: Scalar): Matrix = m * invert(s)
    
    extension (m: Matrix)
        @targetName("divideMS_Op")
        def /(s: Scalar): Matrix = divideMS(m, s)

    def divideCVS(cv: ColumnVector, s: Scalar): ColumnVector = cv * invert(s)
    
    extension (v: ColumnVector)
        @targetName("divideCVS_Op")
        def /(s: Scalar): ColumnVector = divideCVS(v, s)

    def divideRVS(v: RowVector, s: Scalar): RowVector = v * invert(s)

    extension (v: RowVector)
        @targetName("divideRVS_Op")
        def /(s: Scalar): RowVector = divideRVS(v, s)

    def divideSM(s: Scalar, m: Matrix): Matrix = divideMS(m, s)
    def divideSCV(s: Scalar, v: ColumnVector): ColumnVector = divideCVS(v, s)
    def divideSRV(s: Scalar, v: RowVector): RowVector = divideRVS(v, s)
    def divideSS(s1: Scalar, s2: Scalar): Scalar = s1 * invert(s2)

    extension (s: Scalar)
        @targetName("divideSM_Op")
        def /(m: Matrix): Matrix = divideSM(s, m)
        @targetName("divideSCV_Op")
        def /(v: ColumnVector): ColumnVector = divideSCV(s, v)
        @targetName("divideSRV_Op")
        def /(v: RowVector): RowVector = divideSRV(s, v)
        @targetName("divideSS_Op")
        def /(s2: Scalar): Scalar = divideSS(s, s2)

    
    def elementWiseMultiplyMM(m1: Matrix, m2: Matrix): Matrix

    extension (m: Matrix)
        @targetName("elementWiseMultiplyMM_Op")
        def *:*(m2: Matrix): Matrix = elementWiseMultiplyMM(m, m2)

    def elementWiseMultiplyCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector
        
    extension (cv: ColumnVector)
        @targetName("elementWiseMultiplyCVCV_Op")
        def *:*(cv2: ColumnVector): ColumnVector = elementWiseMultiplyCVCV(cv, cv2)
    
    def elementWiseMultiplyRVRV(rv1: RowVector, rv2: RowVector): RowVector = (rv1.t *:* rv2.t).t

    extension (rv: RowVector)
        @targetName("elementWiseMultiplyRVRV_Op")
        def *:*(rv2: RowVector): RowVector = elementWiseMultiplyRVRV(rv, rv2)


    def multiplyMS(m: Matrix, s: Scalar): Matrix
    def multiplyCVS(cv: ColumnVector, s: Scalar): ColumnVector
    def multiplyRVS(rv: RowVector, s: Scalar): RowVector = (rv.t * s).t
    def multiplySS(s1: Scalar, s2: Scalar): Scalar

    extension (m: Matrix)
        @targetName("multiplyMS_Op")
        def *(s: Scalar): Matrix = multiplyMS(m, s)

    extension (cv: ColumnVector)
        @targetName("multiplyCVS_Op")
        def *(s: Scalar): ColumnVector = multiplyCVS(cv, s)

    extension (rv: RowVector)
        @targetName("multiplyRVS_Op")
        def *(s: Scalar): RowVector = multiplyRVS(rv, s)

    def multiplySM(s: Scalar, m: Matrix): Matrix = multiplyMS(m, s)
    def multiplySCV(s: Scalar, cv: ColumnVector): ColumnVector = multiplyCVS(cv, s)
    def multiplySRV(s: Scalar, rv: RowVector): RowVector = multiplyRVS(rv, s)

    extension (s: Scalar)
        @targetName("multiplySM_Op")
        def *(m: Matrix): Matrix = multiplySM(s, m)
        @targetName("multiplySCV_Op")
        def *(cv: ColumnVector): ColumnVector = multiplySCV(s, cv)
        @targetName("multiplySRV_Op")
        def *(rv: RowVector): RowVector = multiplySRV(s, rv)
        @targetName("multiplySS_Op")
        def *(s2: Scalar): Scalar = multiplySS(s, s2)
