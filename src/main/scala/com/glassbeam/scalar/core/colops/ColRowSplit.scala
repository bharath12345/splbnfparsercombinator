package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColRowSplit extends Logger {
  private final lazy val logger = Logging(this)
}

class ColRowSplit(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColRowSplit._

  private var rowsplitOps = 0
  private var rowsplitIter: Iterator[String] = null

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case ROWSPLIT => // /pattern/, src-col
      if (colparam.size != 2 || !colparam(0).isInstanceOf[RegexColumnParameter] || !colparam(1).isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"ROWSPLIT must have two parameters, a pattern and a column, l# $splline")
      } else if (rowsplitOps > 1) {
        throw new Exception(s"ROWSPLIT can only be used once inside a table, l# $splline")
      } else
        rowsplitOps += 1 // Only one ROWSPLIT per table

      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      try {
        val reg = colparam.head.regex
        if (!COS.seenRowsplit && ColString(colparam(1)).isDefined) {
          val splits = reg.findAllIn(ColString(colparam(1)).get).toArray
          rowsplitIter = splits.iterator
          COS.rowsplitIterlen = splits.length
          COS.seenRowsplit = true
        }
        if (rowsplitIter != null && rowsplitIter.hasNext)
          colparam(1).column.setValue(StringValue(rowsplitIter.next()))
        COS.rowsplitIterlen -= 1
        if (COS.rowsplitIterlen <= 0) COS.seenRowsplit = false // Reset for next ROWSPLIT line
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("ROWSPLIT", Option(e)), true)
      }
  }
}