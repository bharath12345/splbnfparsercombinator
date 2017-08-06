package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
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

class ColMap(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColMap._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLMAP(destCol, testCol. /pattern/, col1|literal1, col2|literal2)
    // if destCol matches pattern, take col1|literal1, else take col2|literal2
    case COLMAP =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter] || !colparam(1).isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLMAP 1st & 2nd parameter must both be COLUMNS, l# $splline")
        colerror = true
      }

      if (colparam.size < 4) {
        SM.error(s"COLMAP must have at least four parameters, l# $splline")
        colerror = true
      }

      // There's very little that can be prepared for COLMAP...

      if (colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
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
          colparam.head.column.setValue(colparam(p + 1).getValue)
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLMAP", Option(e)), true)
      }
  }
}