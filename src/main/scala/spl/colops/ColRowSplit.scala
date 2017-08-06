package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColRowSplit extends Logger {
  private final lazy val logger = Logging(this)
}

class ColRowSplit(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColRowSplit._

  private var rowsplitOps = 0
  private var rowsplitIter: Iterator[String] = null

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case ROWSPLIT => // /pattern/, src-col
      var colerror = false
      if (colparam.size != 2 || !colparam(0).isInstanceOf[RegexColumnParameter] || !colparam(1).isInstanceOf[ColColumnParameter]) {
        SM.error(s"ROWSPLIT must have two parameters, a pattern and a column, l# $splline")
        colerror = true
      } else if (rowsplitOps > 1) {
        SM.error(s"ROWSPLIT can only be used once inside a table, l# $splline")
        colerror = true
      } else
        rowsplitOps += 1 // Only one ROWSPLIT per table

      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
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