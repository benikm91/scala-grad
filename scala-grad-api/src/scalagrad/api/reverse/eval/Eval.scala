package scalagrad.api.reverse.eval

import scalagrad.api.reverse.delta.*
import scalagrad.api.matrixalgebra.MatrixAlgebra

case class Eval[PScalar, PColumnVector, PRowVector, PMatrix](
    pma: MatrixAlgebra[PScalar, PColumnVector, PRowVector, PMatrix]
):

    import pma.*

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

    def evalScalar(dOutput: PScalar, delta: DeltaScalarT): Results = 
        assert(delta.index != -1)
        eval(EvalStepScalarResult(dOutput, delta), delta.index)

    def eval(
        start: EvalStepResult,
        endIndex: Int,
    ): Results = 
        
        var results = Results.empty

        if (endIndex == -1) return results

        val intermediateResults = Array.ofDim[EvalStepResult](endIndex + 1)
        intermediateResults(endIndex) = start

        var i = endIndex
        while(i >= 0) {
            intermediateResults(i) match {
                case null => // skip
                case EvalStepScalarResult(dOutput, ds) =>
                    ds.index = -1
                    results = evalScalarStep(dOutput, ds, intermediateResults, endIndex, results)
                case EvalStepColumnVectorResult(dOutput, dcv) =>
                    dcv.index = -1
                    results = evalColumnVectorStep(dOutput, dcv, intermediateResults, endIndex, results)
                case EvalStepRowVectorResult(dOutput, drv) =>
                    drv.index = -1
                    results = evalRowVectorStep(dOutput, drv, intermediateResults, endIndex, results)
                case EvalStepMatrixResult(dOutput, dm) =>
                    dm.index = -1
                    results = evalMatrixStep(dOutput, dm, intermediateResults, endIndex, results)
            }
            intermediateResults(i) = null // clear
            i -= 1
        }

        results

    private def storeIntoIntermediateResults(
        dOutput: PScalar,
        delta: DeltaScalarT,
        intermediateResults: Array[EvalStepResult],
    ): Unit =
        val index = delta.index
        intermediateResults(index) =
            Option(intermediateResults(index).asInstanceOf[EvalStepScalarResult]).fold(
                EvalStepScalarResult(dOutput, delta)
            )(x =>
                assert(x.ds.index == delta.index)
                x.copy(dOutput = x.dOutput + dOutput)
            )
    
    private def evalScalarStep(
        dOutput: PScalar, 
        delta: DeltaScalarT,
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results,
    ): Results =
        if (delta.index != -1) then
            storeIntoIntermediateResults(dOutput, delta, intermediateResults)
            results
        else
            import DeltaScalar.*
            delta match
                case Zero() => results
                case Val(id) =>
                    results.copy(
                        scalars =
                            results.scalars.updatedWith(id)(v => Some(
                                v.fold(dOutput)(_ + dOutput)
                            ))
                    )
                case ElementAtM(dm, iRow, jColumn, nRows, nCols) => 
                    val index = dm.index
                    lazy val newOutput = {
                        val newOutput = zeroMatrix(nRows, nCols)
                        newOutput.setElementAt(iRow, jColumn, dOutput)
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dm.isInstanceOf[DeltaMatrix.Val[PScalar, PColumnVector, PRowVector, PMatrix]])
                        evalMatrixStep(newOutput, dm, intermediateResults, endIndex, results)
                    } else {
                        // Store change in intermediateResults
                        intermediateResults(index) = Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                            EvalStepMatrixResult(newOutput, dm)
                        )(x =>
                            assert(x.dm.index == dm.index)
                            val existingOutput = x.dOutput.setElementAt(iRow, jColumn, x.dOutput.elementAt(iRow, jColumn) + dOutput)
                            x.copy(dOutput = existingOutput)
                        )
                        results
                    }
                case ElementAtCV(dcv, iRow, length) => 
                    val index = dcv.index
                    lazy val newOutput = {
                        val newOutput = zeroColumnVector(length)
                        newOutput.setElementAt(iRow, dOutput)
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dcv.isInstanceOf[DeltaColumnVector.Val[PScalar, PColumnVector, PRowVector, PMatrix]])
                        evalColumnVectorStep(newOutput, dcv, intermediateResults, endIndex, results)
                    } else {
                        intermediateResults(index) = 
                            Option(intermediateResults(index).asInstanceOf[EvalStepColumnVectorResult]).fold(
                                EvalStepColumnVectorResult(newOutput, dcv)
                            )(x =>
                                assert(x.dcv.index == dcv.index)
                                val existingOutput = x.dOutput.setElementAt(iRow, dOutput)
                                x.copy(dOutput = existingOutput)
                            )
                        results
                    }
                case NegateS(s) => 
                    evalScalarStep(-dOutput, s, intermediateResults, endIndex, results)
                case Invert(s) => 
                    evalScalarStep(invert(dOutput), s, intermediateResults, endIndex, results)
                case SumCV(dcv, length) => 
                    evalColumnVectorStep(
                        pma.createColumnVectorFromElements(Vector.fill(length)(dOutput)), 
                        dcv, intermediateResults, endIndex, results
                    )
                case SumM(dm, nRows, nCols) => 
                    evalMatrixStep(
                        pma.createMatrixFromElements(nRows, nCols, Vector.fill(nRows * nCols)(dOutput)), 
                        dm, intermediateResults, endIndex, results
                    )
                case PlusDSDS(ds1, ds2) => 
                    evalScalarStep(dOutput, ds1, intermediateResults, endIndex, 
                        evalScalarStep(dOutput, ds2, intermediateResults, endIndex, results)
                    )
                case DotRVDCV(rv, dcv) => 
                    evalColumnVectorStep(dOutput * rv.t, dcv, intermediateResults, endIndex, results)
                case DotDRVCV(drv, cv) => 
                    evalRowVectorStep((dOutput * cv).t, drv, intermediateResults, endIndex, results)
                case MultiplySDS(s, ds) => 
                    evalScalarStep(dOutput * s, ds, intermediateResults, endIndex, results)

    private def storeIntoIntermediateResults(
        dOutput: PColumnVector,
        delta: DeltaColumnVectorT,
        intermediateResults: Array[EvalStepResult],
    ): Unit =
        val index = delta.index
        intermediateResults(index) =
            Option(intermediateResults(index).asInstanceOf[EvalStepColumnVectorResult]).fold(
                EvalStepColumnVectorResult(dOutput, delta)
            )(x =>
                assert(x.dcv.index == delta.index)
                x.copy(dOutput = x.dOutput + dOutput)
            )

    private def evalColumnVectorStep(
        dOutput: PColumnVector,
        delta: DeltaColumnVectorT,
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results,
    ): Results =
        if (delta.index != -1) then
            val index = delta.index
            storeIntoIntermediateResults(dOutput, delta, intermediateResults)
            results
        else
            import DeltaColumnVector.*
            delta match
                case Zero() => results
                case Val(id) =>
                    results.copy(
                        columnVectors =
                            results.columnVectors.updatedWith(id)(v => Some(
                                v.fold(dOutput)(_ + dOutput)
                            ))
                    )
                case ColumnAtM(dm, jColumn, nRows, nColumns) => 
                    val index = dm.index
                    lazy val newOutput = {
                        val newOutput = pma.zeroMatrix(nRows, nColumns)
                        newOutput.setColumnAt(jColumn, dOutput)
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dm.isInstanceOf[DeltaMatrix.Val[PScalar, PColumnVector, PRowVector, PMatrix]])
                        evalMatrixStep(newOutput, dm, intermediateResults, endIndex, results)
                    } else {
                        intermediateResults(index) =
                            Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                                EvalStepMatrixResult(newOutput, dm)
                            )(x =>
                                assert(x.dm.index == dm.index)
                                x.copy(dOutput = x.dOutput.setColumnAt(jColumn, x.dOutput.columnAt(jColumn) + dOutput))
                            )
                        results
                    }
                case NegateCV(dcv) => 
                    evalColumnVectorStep(-dOutput, dcv, intermediateResults, endIndex, results)
                case TransposeRV(drv) => 
                    evalRowVectorStep(dOutput.t, drv, intermediateResults, endIndex, results)
                case CreateCV(elements) => 
                    elements.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dScalar, i) = t
                        evalScalarStep(dOutput.elementAt(i), dScalar, intermediateResults, endIndex, results)
                    )
                case PlusDCVDCV(dcv1, dcv2) => 
                    evalColumnVectorStep(dOutput, dcv1, intermediateResults, endIndex, 
                        evalColumnVectorStep(dOutput, dcv2, intermediateResults, endIndex, results)
                    )
                case PlusDCVDS(dcv, ds) => 
                    evalColumnVectorStep(dOutput, dcv, intermediateResults, endIndex, 
                        dOutput.foldLeft(results)(
                            (acc, x) => evalScalarStep(x, ds, intermediateResults, endIndex, acc)
                        )
                    )
                case DotMDCV(m, dcv) => 
                    evalColumnVectorStep(m.t * dOutput, dcv, intermediateResults, endIndex, results)
                case DotDMCV(dm, cv) => 
                    evalMatrixStep(dOutput * cv.t, dm, intermediateResults, endIndex, results)
                case MultiplyCVDS(cv, ds) => 
                    evalScalarStep((dOutput *:* cv).sum, ds, intermediateResults, endIndex, results)
                case MultiplyDCVS(dcv, s) => 
                    evalColumnVectorStep(dOutput * s, dcv, intermediateResults, endIndex, results)
                case ElementWiseMultiplyCVDCV(cv, dcv) => 
                    evalColumnVectorStep(dOutput *:* cv, dcv, intermediateResults, endIndex, results)
                case SetElementAtCV(dcv, i, ds) =>
                    val dOutputDs = dOutput.elementAt(i)
                    evalColumnVectorStep(dOutput.setElementAt(i, zeroScalar), dcv, intermediateResults, endIndex, 
                        evalScalarStep(dOutputDs, ds, intermediateResults, endIndex, results)
                    )

    private def storeIntoIntermediateResults(
        dOutput: PRowVector,
        delta: DeltaRowVectorT,
        intermediateResults: Array[EvalStepResult],
    ): Unit =
        val index = delta.index
        intermediateResults(index) =
            Option(intermediateResults(index).asInstanceOf[EvalStepRowVectorResult]).fold(
                EvalStepRowVectorResult(dOutput, delta)
            )(x =>
                assert(x.drv.index == delta.index)
                x.copy(dOutput = x.dOutput + dOutput)
            )

    def evalRowVectorStep(
        output: PRowVector,
        delta: DeltaRowVectorT,
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results,
    ): Results =
        if (delta.index != -1) then
            val index = delta.index
            storeIntoIntermediateResults(output, delta, intermediateResults)
            results
        else
            import DeltaRowVector.*
            delta match
                case Zero() => results
                case Val(id) =>
                    results.copy(
                        rowVectors =
                            results.rowVectors.updatedWith(id)(v => Some(
                                v.fold(output)(_ + output)
                            ))
                    )
                case NegateRV(drv) => 
                    evalRowVectorStep(-output, drv, intermediateResults, endIndex, results)
                case TransposeCV(dcv) => 
                    evalColumnVectorStep(output.t, dcv, intermediateResults, endIndex, results)

    private def storeIntoIntermediateResults(
                                              dOutput: PMatrix,
                                              delta: DeltaMatrixT,
                                              intermediateResults: Array[EvalStepResult],
                                            ): Unit =
        val index = delta.index
        intermediateResults(index) =
            Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                EvalStepMatrixResult(dOutput, delta)
            )(x =>
                assert(x.dm.index == delta.index, f"${x.dm.index} != ${delta.index}")
                x.copy(dOutput = x.dOutput + dOutput)
            )

    def evalMatrixStep(
        dOutput: PMatrix,
        delta: DeltaMatrixT,
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results,
    ): Results =
        if (delta.index != -1) then
            val index = delta.index
            storeIntoIntermediateResults(dOutput, delta, intermediateResults)
            results
        else
            import DeltaMatrix.*
            delta match
                case Zero() => results
                case Val(id) =>
                    results.copy(
                        matrices =
                            results.matrices.updatedWith(id)(v => Some(
                                v.fold(dOutput)(_ + dOutput)
                            ))
                    )
                case NegateM(dm) => 
                    evalMatrixStep(-dOutput, dm, intermediateResults, endIndex, results)
                case TransposeM(dm) => 
                    evalMatrixStep(dOutput.t, dm, intermediateResults, endIndex, results)
                case CreateM(nRows, nCols, elements) => 
                    elements.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dScalar, i) = t
                        val (jCol, iRow) = (i / dOutput.nRows, i % dOutput.nRows)
                        evalScalarStep(dOutput.elementAt(iRow, jCol), dScalar, intermediateResults, endIndex, results)
                    )
                case StackColumns(columns) => ???
                case PlusDMDM(dm1, dm2) => 
                    evalMatrixStep(dOutput, dm1, intermediateResults, endIndex, 
                        evalMatrixStep(dOutput, dm2, intermediateResults, endIndex, results)
                    )
                case PlusDMDCV(dm, dcv) => 
                    evalMatrixStep(dOutput, dm, intermediateResults, endIndex, 
                        evalColumnVectorStep(dOutput.reduceRows(_.sum), dcv, intermediateResults, endIndex, results)
                    )
                case PlusDMDS(dm, ds) => 
                    evalMatrixStep(dOutput, dm, intermediateResults, endIndex, 
                        dOutput.foldLeft(results)(
                            (acc, x) => evalScalarStep(x, ds, intermediateResults, endIndex, acc)
                        )
                    )
                case DotMDM(m, dm) => 
                    evalMatrixStep(m.t * dOutput, dm, intermediateResults, endIndex, results)
                case DotDMM(dm, m) => 
                    evalMatrixStep(dOutput * m.t, dm, intermediateResults, endIndex, results)
                case DotDCVRV(dcv, rv) => 
                    evalColumnVectorStep(dOutput * rv.t, dcv, intermediateResults, endIndex, results)
                case DotCVDRV(cv, drv) => 
                    evalRowVectorStep(cv.t * dOutput, drv, intermediateResults, endIndex, results)
                case MultiplyMDS(m, ds) => 
                    evalScalarStep((dOutput *:* m).sum, ds, intermediateResults, endIndex, results)
                case MultiplyDMS(dm, s) => 
                    evalMatrixStep(dOutput * s, dm, intermediateResults, endIndex, results)
                case ElementWiseMultiplyMDM(m, dm) => 
                    evalMatrixStep(dOutput *:* m, dm, intermediateResults, endIndex, results)
                case SetElementAtM(dm, iRow: Int, jColumn: Int, ds) => 
                    val dOutputDs = dOutput.elementAt(iRow, jColumn)
                    evalMatrixStep(
                        dOutput.setElementAt(iRow, jColumn, zeroScalar), dm, intermediateResults, endIndex,
                        evalScalarStep(dOutputDs, ds, intermediateResults, endIndex, results)
                    )
                case SetColumnAtM(dm, jColumn, dcv, dcvLength) =>
                    val dOutputCv = dOutput.columnAt(jColumn) 
                    evalMatrixStep(
                        dOutput.setColumnAt(jColumn, zeroColumnVector(dcvLength)), dm, intermediateResults, endIndex,
                        evalColumnVectorStep(dOutputCv, dcv, intermediateResults, endIndex, results)
                    )

    case class Results(
        scalars: Map[Int, PScalar],
        columnVectors: Map[Int, PColumnVector],
        rowVectors: Map[Int, PRowVector],
        matrices: Map[Int, PMatrix]
    )

    object Results:
        def empty: Results = Results(Map(), Map(), Map(), Map())
