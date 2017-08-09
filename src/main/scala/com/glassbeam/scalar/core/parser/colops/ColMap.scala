package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger}
import com.glassbeam.scalar.utils.MatchUtils._

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColMap extends Logger {
  private final lazy val logger = Logging(this)
}

class ColMap(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColMap._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLMAP(destCol, testCol. /pattern/, col1|literal1, col2|literal2)
    // if destCol matches pattern, take col1|literal1, else take col2|literal2
    case COLMAP =>
      if (!colparam.head.isInstanceOf[ColColumnParameter] || !colparam(1).isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLMAP 1st & 2nd parameter must both be COLUMNS, l# $splline")
      }

      if (colparam.size < 4) {
        throw new Exception(s"COLMAP must have at least four parameters, l# $splline")
      }

      // There's very little that can be prepared for COLMAP...
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      try {
        var p = 2
        val src: DataValue = colparam(1).getValue
        var found = false
        do {
          if (p + 1 == colparam.size) {
            // Final else literal, if available
            p -= 1
            found = true
          } else {
            if (colparam(p).isInstanceOf[RegexColumnParameter]) {
              if (src != EmptyValue && matchFound(colparam(p).regex, src.toString)) found = true
            } else if (src != EmptyValue && ColString(colparam(p)) == Option(src.toString)) found = true // Case sensitive literal matching
          }

          if (!found) {
            if (p + 2 >= colparam.size)
              logger.debug(SM.mpspath, "COLMAP(" + splline + ") -- ran out of options, logline=" + src)
            p += 2
          }
        } while (!found && p < colparam.size)
        if (p + 1 < colparam.size) {
          val column = getColumnForCOLUMN(colparam.head.column, COS)
          val colp = getColumnForCOLUMN(colparam(p + 1).column, COS)
          column.setValue(colp.getValue)
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLMAP", Option(e)), true)
      }
  }
}