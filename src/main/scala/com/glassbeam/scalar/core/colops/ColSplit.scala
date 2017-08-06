package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColSplit extends Logger {
  private final lazy val logger = Logging(this)
}

class ColSplit(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColSplit._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLSPLIT(srcCol, /pattern-w-backref/, destCol1 [, destColN])
    case COLSPLIT => //  col, regex-w-backrefs, col1, ...
      var colerror = false
      if (colparam.size < 3) {
        SM.error(s"COLSPLIT must have at least three parameters, l# $splline")
        colerror = true
      } else {
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          SM.error(s"COLSPLIT first parameter must be COLUMN, l# $splline")
          colerror = true
        }
        if (!colparam(1).isInstanceOf[RegexColumnParameter]) {
          SM.error(s"COLSPLIT second parameter must be a pattern, l# $splline")
          colerror = true
        }
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      try {
        if(colparam.head.getValue.isEmpty) {
          logger.warning(s"Colsplit incoming column is empty. splline = $splline")
        } else {
          val src = DataValue.getStringValue(colparam.head.getValue)
          val rx = colparam(1).regex
          logger.debug(SM.mpspath, s"COLSPLIT($splline): src=$src rx=$rx, table = ${COS.table_name}")
          rx findFirstMatchIn src match {
            case Some(m) =>
              var i = 1
              for (c <- colparam.drop(2)) {
                try {
                  c.column.setValue(StringValue(m.group(i)))
                  logger.debug(SM.mpspath, s"COLSPLIT: table = ${COS.table_name}, col = ${c.name}, " +
                    s"param $i = ${m.group(i)}")
                } catch {
                  case NonFatal(e) =>
                    logger.warning(SM.mpspath, warningString(s"COLSPLIT for param $i no match, rx=$rx, src=$src", Option(e)))
                }
                i += 1
              }
            case None =>
              for (c <- colparam.drop(2))
                c.setValue(EmptyValue)
              logger.debug(SM.mpspath, s"COLSPLIT($splline) no match, rx=$rx, src=$src, table = ${COS.table_name}")
          }
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString(s"COLSPLIT", Option(e)), true)
      }
  }
}