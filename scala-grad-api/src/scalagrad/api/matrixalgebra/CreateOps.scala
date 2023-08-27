package scalagrad.api.matrixalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits.*

trait CreateOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: TransposeOps[ColumnVector, RowVector, Matrix] =>

    def createMatrixFromElements(nRows: Int, nCols: Int, elements: Seq[Scalar]): Matrix
    def stackRows(rows: Seq[RowVector]): Matrix = stackColumns(rows.map(_.t)).t
    def stackColumns(columns: Seq[ColumnVector]): Matrix
    def createColumnVectorFromElements(elements: Seq[Scalar]): ColumnVector
    def createRowVectorFromElements(elements: Seq[Scalar]): RowVector = createColumnVectorFromElements(elements).t