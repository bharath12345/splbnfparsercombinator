package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.model._

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColRep extends Logger {
  private final lazy val logger = Logging(this)
}

class ColRep(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColRep._

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLREP(/pattern/, STRING, col)
    case COLREP => // /regex/, 'string', column
      if (!colparam(0).isInstanceOf[RegexColumnParameter]) {
        throw new Exception(s"COLREP first parameter must be a pattern, l# $splline")
      }

      if (!colparam(2).isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLREP third parameter must be a COLUMN, l# $splline")
      }

      val column = colparam(2).asInstanceOf[ColColumnParameter]
      // ToDo: ColRep is badly designed column function. It does in-place mutation of column value. Thats why the below
      // ToDo: check is not permissible. Since a column can hold a string value which is like "0 1 2" which is mutated to "012"
      // ToDo: and written - and all the while the column type is Integer!
      //if(column.typ != ColumnType.STRING) {
      //  error(s"COLREP third parameter must be a STRING COLUMN (it is non STRING), l# $splline")
      //  colerror = true
      //}

      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
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