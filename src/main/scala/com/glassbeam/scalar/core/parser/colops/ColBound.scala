package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model._
import com.glassbeam.scalar.core.parser.Methods._
import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.parser.Methods
import ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColBound extends Logger {
  private final lazy val logger = Logging(this)
}

class ColBound(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {
  import ColBound._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLBOUND =>
      logger.debug(SIM.mpspath, s"COLBOUND, colparam = $colparam, param = $param")
      ColString(colparam(1)) match {
        case Some(cbnd) =>
          val m = Methods.withName(cbnd)
          if (!colparam.head.isInstanceOf[ColColumnParameter]) {
            throw new Exception(s"COLBOUND 1st argument must be COLUMN, l# $splline")
          }

        case None => {
          throw new Exception(s"COLBOUND 1st argument must be COLUMN, l# $splline")
        }
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      try {
        val column = getColumnForCOLUMN(colparam.head.column, COS)
        val t =
          Methods.withName(ColString(colparam(1)).get) match {
            case FIRST =>
              for {
                src <- ColString(colparam(2))
                if column.getValue.isEmpty || column.getValue.toString.isEmpty
              } column.setValue(StringValue(src))

            case LAST =>
              ColString(colparam(2)).foreach { src =>
                column.setValue(StringValue(src))
              }

            case MIN =>
              column.typ match {
                case ColumnType.STRING =>
                  ColString(colparam(2)).foreach { src =>
                    if (column.getValue.nonEmpty && DataValue.getStringValue(column.getValue) > src)
                      column.setValue(StringValue(src))
                  }
                case ColumnType.INTEGER | ColumnType.LONG =>
                  ColString(colparam(2)).foreach { src =>
                    val n = NumericLong(src)
                    if (column.getValue.nonEmpty && NumericLong(DataValue.getStringValue(column.getValue)) > n)
                      column.setValue(LongValue(n))
                  }
                case ColumnType.FLOAT | ColumnType.DOUBLE =>
                  ColString(colparam(2)).foreach { src =>
                    val d = NumericDouble(src)
                    if (column.getValue.nonEmpty && NumericDouble(DataValue.getStringValue(column.getValue)) > d)
                      column.setValue(DoubleValue(d))
                  }
              }

            case MAX =>
              column.typ match {
                case ColumnType.STRING =>
                  ColString(colparam(2)).foreach { src =>
                    if (column.getValue.nonEmpty && DataValue.getStringValue(column.getValue) < src)
                      column.setValue(StringValue(src))
                  }
                case ColumnType.INTEGER | ColumnType.LONG =>
                  ColString(colparam(2)).foreach { src =>
                    val n = NumericLong(src)
                    if (column.getValue.nonEmpty && NumericLong(DataValue.getStringValue(column.getValue)) < n)
                      column.setValue(LongValue(n))
                  }
                case ColumnType.FLOAT | ColumnType.DOUBLE =>
                  ColString(colparam(2)).foreach { src =>
                    val d = NumericDouble(src)
                    if (column.getValue.nonEmpty && NumericDouble(DataValue.getStringValue(column.getValue)) < d)
                      column.setValue(DoubleValue(d))
                  }
              }
          }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLBOUND", Option(e)), true)
      }
  }
}
