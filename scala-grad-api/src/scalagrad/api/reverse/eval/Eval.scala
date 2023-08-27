package scalagrad.api.reverse.eval

import scalagrad.api.matrixalgebra.MatrixAlgebra
import scalagrad.api.reverse.delta.*

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

case class Eval[PScalar, PColumnVector, PRowVector, PMatrix](
    pma: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix]
):

    import pma.*

    case class PrioritySetQueue[K](
        po: PriorityQueue[K],
        hs: mutable.Set[Int]
    ):
        def nonEmpty = po.nonEmpty
        def dequeue() = {
            val res = po.dequeue()
            hs.remove(toHashCode(res))
            res
        }
        def addOne(elem: K): Boolean = 
            if (!hs.contains(toHashCode(elem))) {
                po.addOne(elem)
                hs.add(toHashCode(elem))
                return true
            }
            return false

    case class DOutputs(
        val scalar: mutable.Map[Int, PScalar] = mutable.Map(),
        val columnVector: mutable.Map[Int, PColumnVector] = mutable.Map(),
        val rowVector: mutable.Map[Int, PRowVector] = mutable.Map(),
        val matrix: mutable.Map[Int, PMatrix] = mutable.Map(),
    )

    type DeltaScalarT = DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]
    type DeltaColumnVectorT = DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DeltaRowVectorT = DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]
    type DeltaMatrixT = DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]

    enum EvalStepResult:
        case EvalStepScalarResult(dOutput: PScalar, ds: DeltaScalarT)
        case EvalStepColumnVectorResult(dOutput: PColumnVector, dcv: DeltaColumnVectorT)
        case EvalStepRowVectorResult(dOutput: PRowVector, drv: DeltaRowVectorT)
        case EvalStepMatrixResult(dOutput: PMatrix, dm: DeltaMatrixT)

    import EvalStepResult.*

    def toHashCode(x: Any) = System.identityHashCode(x)

    def evalScalar(dOutput: PScalar, delta: DeltaScalar[PScalar, PColumnVector, PRowVector, PMatrix]): Results = 
        eval(DOutputs(scalar = mutable.Map(toHashCode(delta) -> dOutput)), delta.asInstanceOf[DeltaVal])

    def evalColumnVector(dOutput: PColumnVector, delta: DeltaColumnVector[PScalar, PColumnVector, PRowVector, PMatrix]): Results = 
        eval(DOutputs(columnVector = mutable.Map(toHashCode(delta) -> dOutput)), delta.asInstanceOf[DeltaVal])

    def evalRowVector(dOutput: PRowVector, delta: DeltaRowVector[PScalar, PColumnVector, PRowVector, PMatrix]): Results = 
        eval(DOutputs(rowVector = mutable.Map(toHashCode(delta) -> dOutput)), delta.asInstanceOf[DeltaVal])

    def evalMatrix(dOutput: PMatrix, delta: DeltaMatrix[PScalar, PColumnVector, PRowVector, PMatrix]): Results = 
        eval(DOutputs(matrix = mutable.Map(toHashCode(delta) -> dOutput)), delta.asInstanceOf[DeltaVal])

    def eval(
        dOutputs: DOutputs,
        startVal: DeltaVal
    ): Results = 
        
        val nextDeltas = PrioritySetQueue(
            PriorityQueue[DeltaVal]()(Ordering.by[DeltaVal, Int](_.index)),
            mutable.HashSet[Int]()
        )
        var currentIndex = startVal.index
        nextDeltas.addOne(startVal)

        val results = Results.empty

        while(nextDeltas.nonEmpty) {
            nextDeltas.dequeue() match {
                case dsVal @ DeltaScalar.Val[PScalar, PColumnVector, PRowVector, PMatrix](nextIndex, ds) =>
                    assert(currentIndex >= nextIndex, f"$currentIndex >= $nextIndex")
                    currentIndex = nextIndex
                    val dOutput = dOutputs.scalar.remove(toHashCode(dsVal)).get
                    evalScalarStep(dOutput, ds, results, dOutputs, nextDeltas)
                case dcvVal @ DeltaColumnVector.Val[PScalar, PColumnVector, PRowVector, PMatrix](nextIndex, dcv) =>
                    assert(currentIndex >= nextIndex, f"$currentIndex >= $nextIndex")
                    currentIndex = nextIndex
                    val dOutput = dOutputs.columnVector.remove(toHashCode(dcvVal)).get
                    evalColumnVectorStep(dOutput, dcv, results, dOutputs, nextDeltas)
                case drvVal @ DeltaRowVector.Val[PScalar, PColumnVector, PRowVector, PMatrix](nextIndex, drv) =>
                    assert(currentIndex >= nextIndex, f"$currentIndex >= $nextIndex")
                    currentIndex = nextIndex
                    val dOutput = dOutputs.rowVector.remove(toHashCode(drvVal)).get
                    evalRowVectorStep(dOutput, drv, results, dOutputs, nextDeltas)
                case dmVal @ DeltaMatrix.Val[PScalar, PColumnVector, PRowVector, PMatrix](nextIndex, dm) =>
                    assert(currentIndex >= nextIndex, f"$currentIndex >= $nextIndex")
                    currentIndex = nextIndex
                    val dOutput = dOutputs.matrix.remove(toHashCode(dmVal)).get
                    evalMatrixStep(dOutput, dm, results, dOutputs, nextDeltas)
            }
        }

        results
    
    private def evalScalarStep(
        dOutput: PScalar, 
        delta: DeltaScalarT,
        results: Results,
        dOutputs: DOutputs,
        nextDeltas: PrioritySetQueue[DeltaVal],
    ): Unit =
        import DeltaScalar.*
        delta match
            case Zero() => ()
            case Input(id) =>
                results.scalars.updateWith(id)(v => Some(
                    v.fold(dOutput)(_ + dOutput)
                ))
            case ds @ Val(_, _) =>
                dOutputs.scalar.updateWith(toHashCode(ds))(v => Some(
                    v.fold(dOutput)(_ + dOutput)
                ))
                nextDeltas.addOne(ds)
            case ElementAtM(dm, iRow, jColumn, nRows, nCols) => 
                dOutputs.matrix.updateWith(toHashCode(dm))(v =>
                    val dmOutput = v.getOrElse(zeroMatrix(nRows, nCols))
                    Some(dmOutput.setElementAt(iRow, jColumn, dmOutput.elementAt(iRow, jColumn) + dOutput))
                )
                nextDeltas.addOne(dm.asInstanceOf[DeltaVal])
            case ElementAtCV(dcv, iRow, length) => 
                dOutputs.columnVector.updateWith(toHashCode(dcv))(v =>
                    val dcvOutput = v.getOrElse(zeroColumnVector(length))
                    Some(dcvOutput.setElementAt(iRow, dcvOutput.elementAt(iRow) + dOutput))
                )
                nextDeltas.addOne(dcv.asInstanceOf[DeltaVal])
            case NegateS(ds) => 
                evalScalarStep(-dOutput, ds, results, dOutputs, nextDeltas)
            case SumCV(dcv, length) => 
                evalColumnVectorStep(
                    pma.createColumnVectorFromElements(Vector.fill(length)(dOutput)), 
                    dcv, results, dOutputs, nextDeltas
                )
            case SumM(dm, nRows, nCols) => 
                evalMatrixStep(
                    pma.createMatrixFromElements(nRows, nCols, Vector.fill(nRows * nCols)(dOutput)), 
                    dm, results, dOutputs, nextDeltas
                )
            case PlusDSDS(ds1, ds2) => 
                evalScalarStep(dOutput, ds2, results, dOutputs, nextDeltas)
                evalScalarStep(dOutput, ds1, results, dOutputs, nextDeltas)
            case DotRVDCV(rv, dcv) => 
                evalColumnVectorStep(dOutput * rv.t, dcv, results, dOutputs, nextDeltas)
            case DotDRVCV(drv, cv) => 
                evalRowVectorStep((dOutput * cv).t, drv, results, dOutputs, nextDeltas)
            case MultiplySDS(s, ds) => 
                evalScalarStep(dOutput * s, ds, results, dOutputs, nextDeltas)

    private def evalColumnVectorStep(
        dOutput: PColumnVector,
        delta: DeltaColumnVectorT,
        results: Results,
        dOutputs: DOutputs,
        nextDeltas: PrioritySetQueue[DeltaVal],
    ): Unit =
        import DeltaColumnVector.*
        delta match
            case Zero() => ()
            case Input(id) =>
                results.columnVectors.updateWith(id)(v => Some(
                    v.fold(dOutput)(_ + dOutput)
                ))
            case dcv @ Val(_, _) =>
                dOutputs.columnVector.updateWith(toHashCode(dcv))(cv => Some(
                    cv.fold(dOutput)(_ + dOutput)
                ))
                nextDeltas.addOne(dcv)
            case ColumnAtM(dm, jColumn, nRows, nCols) => 
                dOutputs.matrix.updateWith(toHashCode(dm))(v =>
                    val dmOutput = v.getOrElse(zeroMatrix(nRows, nCols))
                    Some(dmOutput.setColumnAt(jColumn, dmOutput.columnAt(jColumn) + dOutput))
                )
                nextDeltas.addOne(dm.asInstanceOf[DeltaVal])
            case NegateCV(dcv) => 
                evalColumnVectorStep(-dOutput, dcv, results, dOutputs, nextDeltas)
            case TransposeRV(drv) => 
                evalRowVectorStep(dOutput.t, drv, results, dOutputs, nextDeltas)
            case CreateCV(elements) => 
                elements.zipWithIndex.foreach(t => 
                    val (dScalar, i) = t
                    evalScalarStep(dOutput.elementAt(i), dScalar, results, dOutputs, nextDeltas)
                )
            case PlusDCVDCV(dcv1, dcv2) => 
                evalColumnVectorStep(dOutput, dcv2, results, dOutputs, nextDeltas)
                evalColumnVectorStep(dOutput, dcv1, results, dOutputs, nextDeltas)
            case PlusDCVDS(dcv, ds) => 
                dOutput.elements.foreach(x => evalScalarStep(x, ds, results, dOutputs, nextDeltas))
                evalColumnVectorStep(dOutput, dcv, results, dOutputs, nextDeltas)
            case DotMDCV(m, dcv) => 
                evalColumnVectorStep(m.t * dOutput, dcv, results, dOutputs, nextDeltas)
            case DotDMCV(dm, cv) => 
                evalMatrixStep(dOutput * cv.t, dm, results, dOutputs, nextDeltas)
            case MultiplyCVDS(cv, ds) => 
                evalScalarStep((dOutput *:* cv).sum, ds, results, dOutputs, nextDeltas)
            case MultiplyDCVS(dcv, s) => 
                evalColumnVectorStep(dOutput * s, dcv, results, dOutputs, nextDeltas)
            case ElementWiseMultiplyCVDCV(cv, dcv) => 
                evalColumnVectorStep(dOutput *:* cv, dcv, results, dOutputs, nextDeltas)
            case SetElementAtCV(dcv, i, ds) =>
                val dOutputDs = dOutput.elementAt(i)
                evalScalarStep(dOutputDs, ds, results, dOutputs, nextDeltas)
                evalColumnVectorStep(dOutput.setElementAt(i, zeroScalar), dcv, results, dOutputs, nextDeltas)

    def evalRowVectorStep(
        dOutput: PRowVector,
        delta: DeltaRowVectorT,
        results: Results,
        dOutputs: DOutputs,
        nextDeltas: PrioritySetQueue[DeltaVal],
    ): Unit =
        import DeltaRowVector.*
        delta match
            case Zero() => ()
            case Input(id) =>
                results.rowVectors.updateWith(id)(v => Some(
                    v.fold(dOutput)(_ + dOutput)
                ))
            case drv @ Val(_, _) =>
                dOutputs.rowVector.updateWith(toHashCode(drv))(rv => Some(
                    rv.fold(dOutput)(_ + dOutput)
                ))
                nextDeltas.addOne(drv)
            case NegateRV(drv) => 
                evalRowVectorStep(-dOutput, drv, results, dOutputs, nextDeltas)
            case TransposeCV(dcv) => 
                evalColumnVectorStep(dOutput.t, dcv, results, dOutputs, nextDeltas)

    def evalMatrixStep(
        dOutput: PMatrix,
        delta: DeltaMatrixT,
        results: Results,
        dOutputs: DOutputs,
        nextDeltas: PrioritySetQueue[DeltaVal],
    ): Unit =
        import DeltaMatrix.*
        delta match
            case Zero() => ()
            case Input(id) =>
                results.matrices.updateWith(id)(v => Some(
                    v.fold(dOutput)(_ + dOutput)
                ))
            case dm @ Val(_, _) =>
                dOutputs.matrix.updateWith(toHashCode(dm))(m => Some(
                    m.fold(dOutput)(_ + dOutput)
                ))
                nextDeltas.addOne(dm)
            case NegateM(dm) => 
                evalMatrixStep(-dOutput, dm, results, dOutputs, nextDeltas)
            case TransposeM(dm) => 
                evalMatrixStep(dOutput.t, dm, results, dOutputs, nextDeltas)
            case CreateM(nRows, nCols, elements) => 
                elements.zipWithIndex.foreach(t => 
                    val (dScalar, i) = t
                    val (jCol, iRow) = (i / dOutput.nRows, i % dOutput.nRows)
                    evalScalarStep(dOutput.elementAt(iRow, jCol), dScalar, results, dOutputs, nextDeltas)
                )
            case StackColumns(columns) => 
                dOutput.columns.zip(columns).foreach((dcvOutput, dcv) =>
                    evalColumnVectorStep(dcvOutput, dcv, results, dOutputs, nextDeltas)
                )
            case PlusDMDM(dm1, dm2) => 
                evalMatrixStep(dOutput, dm2, results, dOutputs, nextDeltas)
                evalMatrixStep(dOutput, dm1, results, dOutputs, nextDeltas)
            case PlusDMDCV(dm, dcv) => 
                evalColumnVectorStep(dOutput.reduceRows(_.sum), dcv, results, dOutputs, nextDeltas)
                evalMatrixStep(dOutput, dm, results, dOutputs, nextDeltas)
            case PlusDMDS(dm, ds) => 
                evalScalarStep(dOutput.sum, ds, results, dOutputs, nextDeltas)
                evalMatrixStep(dOutput, dm, results, dOutputs, nextDeltas)
            case DotMDM(m, dm) => 
                evalMatrixStep(m.t * dOutput, dm, results, dOutputs, nextDeltas)
            case DotDMM(dm, m) => 
                evalMatrixStep(dOutput * m.t, dm, results, dOutputs, nextDeltas)
            case DotDCVRV(dcv, rv) => 
                evalColumnVectorStep(dOutput * rv.t, dcv, results, dOutputs, nextDeltas)
            case DotCVDRV(cv, drv) => 
                evalRowVectorStep(cv.t * dOutput, drv, results, dOutputs, nextDeltas)
            case MultiplyMDS(m, ds) => 
                evalScalarStep((dOutput *:* m).sum, ds, results, dOutputs, nextDeltas)
            case MultiplyDMS(dm, s) => 
                evalMatrixStep(dOutput * s, dm, results, dOutputs, nextDeltas)
            case ElementWiseMultiplyMDM(m, dm) => 
                evalMatrixStep(dOutput *:* m, dm, results, dOutputs, nextDeltas)
            case SetElementAtM(dm, iRow: Int, jColumn: Int, ds) => 
                val dOutputDs = dOutput.elementAt(iRow, jColumn)
                evalScalarStep(dOutputDs, ds, results, dOutputs, nextDeltas)
                evalMatrixStep(dOutput.setElementAt(iRow, jColumn, zeroScalar), dm, results, dOutputs, nextDeltas)
            case SetColumnAtM(dm, jColumn, dcv, dcvLength) =>
                val dOutputCv = dOutput.columnAt(jColumn) 
                evalColumnVectorStep(dOutputCv, dcv, results, dOutputs, nextDeltas)
                evalMatrixStep(dOutput.setColumnAt(jColumn, zeroColumnVector(dcvLength)), dm, results, dOutputs, nextDeltas)

    case class Results(
        scalars: mutable.Map[Int, PScalar],
        columnVectors: mutable.Map[Int, PColumnVector],
        rowVectors: mutable.Map[Int, PRowVector],
        matrices: mutable.Map[Int, PMatrix]
    )

    object Results:
        def empty: Results = Results(mutable.Map(), mutable.Map(), mutable.Map(), mutable.Map())
