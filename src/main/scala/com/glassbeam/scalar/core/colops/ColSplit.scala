package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColSplit extends Logger {
  private final lazy val logger = Logging(this)
}

class ColSplit(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColSplit._

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLSPLIT(srcCol, /pattern-w-backref/, destCol1 [, destColN])
    case COLSPLIT => //  col, regex-w-backrefs, col1, ...
      if (colparam.size < 3) {
        throw new Exception(s"COLSPLIT must have at least three parameters, l# $splline")
      } else {
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          throw new Exception(s"COLSPLIT first parameter must be COLUMN, l# $splline")
        }
        if (!colparam(1).isInstanceOf[RegexColumnParameter]) {
          throw new Exception(s"COLSPLIT second parameter must be a pattern, l# $splline")
        }
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
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