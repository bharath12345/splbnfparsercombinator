package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.model._

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColRep extends Logger {
  private final lazy val logger = Logging(this)
}

class ColRep(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColRep._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLREP(/pattern/, STRING, col)
    case COLREP => // /regex/, 'string', column
      var colerror = false

      if (!colparam(0).isInstanceOf[RegexColumnParameter]) {
        SM.error(s"COLREP first parameter must be a pattern, l# $splline")
        colerror = true
      }

      if (!colparam(2).isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLREP third parameter must be a COLUMN, l# $splline")
        colerror = true
      }

      val column = colparam(2).asInstanceOf[ColColumnParameter]
      // ToDo: ColRep is badly designed column function. It does in-place mutation of column value. Thats why the below
      // ToDo: check is not permissible. Since a column can hold a string value which is like "0 1 2" which is mutated to "012"
      // ToDo: and written - and all the while the column type is Integer!
      //if(column.typ != ColumnType.STRING) {
      //  error(s"COLREP third parameter must be a STRING COLUMN (it is non STRING), l# $splline")
      //  colerror = true
      //}

      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      val rx = colparam.head.regex
      logger.debug(SM.mpspath, s"COLREP($splline): rx=" + rx)
      for {
        y <- ColString(colparam(1))
        z = colparam(2).getValue
        if z.nonEmpty
      } {
        val replacement: String = z match {
          case LongValue(l) => l.toString
          case StringValue(s) => s
        }
        val value = rx.replaceAllIn(replacement, y)
        colparam(2).column.setValue(StringValue(value))
      }
  }
}