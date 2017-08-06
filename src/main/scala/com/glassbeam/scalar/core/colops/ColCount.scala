package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger, LongValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.utils.MatchUtils._

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCount extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCount(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColCount._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLCOUNT =>
      var colerror = false
      if (colparam.size < 3) {
        SM.error(s"COLCOUNT must have Three parameters, l# $splline")
        colerror = true
      } else if (!(colparam.head.isInstanceOf[ColColumnParameter] &&
        colparam(1).isInstanceOf[RegexColumnParameter] && colparam.last.isInstanceOf[ColColumnParameter])) {
        SM.error(s"COLCOUNT 1st & 3nd parameter must both be COLUMNS, ${colparam.head}, ${colparam(1)}, ${colparam.last}  l# $splline")
        colerror = true
      } else {
        logger.info(s"COLCOUNT compiled successfully Param1 = ${colparam.head}, Param2 = ${colparam(2)}, Param1 = ${colparam.tail}")
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      try {
        if(colparam.head.getValue.isEmpty) {
          logger.error(SM.mpspath, s"Colcount on empty value. splline = $splline", true)
        } else {
          val src = DataValue.getStringValue(colparam.head.getValue)
          val p = colparam(1)
          val found = if (matchFound(p.regex, src)) true else false
          if (found) {
            val column = colparam(2).column
            column.sess_count = column.sess_count + 1
            column.setValue(LongValue(column.sess_count))
          }
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLCOUNT", Option(e)), true)
      }
  }
}

