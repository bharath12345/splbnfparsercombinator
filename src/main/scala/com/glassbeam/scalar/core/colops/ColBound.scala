package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model._
import com.glassbeam.scalar.core.parser.Methods._
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, Methods, SharedImmutables, SharedMutables}
import ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColBound extends Logger {
  private final lazy val logger = Logging(this)
}

class ColBound(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {
  import ColBound._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLBOUND =>
      logger.debug(SM.mpspath, s"COLBOUND, colparam = $colparam, param = $param")
      var colerror = false
      ColString(colparam(1)) match {
        case Some(cbnd) =>
          val m = Methods.withName(cbnd)
          if (!colparam.head.isInstanceOf[ColColumnParameter]) {
            SM.error(s"COLBOUND 1st argument must be COLUMN, l# $splline")
            colerror = true
          }

        case None => {
          SM.error(s"COLBOUND 1st argument must be COLUMN, l# $splline")
          colerror = true
        }
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      try {
        val t =
          Methods.withName(ColString(colparam(1)).get) match {
            case FIRST =>
              for {
                src <- ColString(colparam(2))
                if colparam.head.getValue.isEmpty || colparam.head.getValue.toString.isEmpty
              } colparam.head.column.setValue(StringValue(src))

            case LAST =>
              ColString(colparam(2)).foreach { src =>
                colparam.head.column.setValue(StringValue(src))
              }
            case MIN =>
              colparam.head.typ match {
                case ColumnType.STRING =>
                  ColString(colparam(2)).foreach { src =>
                    if (colparam.head.getValue.nonEmpty && DataValue.getStringValue(colparam.head.getValue) > src)
                      colparam.head.setValue(StringValue(src))
                  }
                case ColumnType.INTEGER | ColumnType.LONG =>
                  ColString(colparam(2)).foreach { src =>
                    val n = NumericLong(src)
                    if (colparam.head.getValue.nonEmpty && NumericLong(DataValue.getStringValue(colparam.head.getValue)) > n)
                      colparam.head.setValue(LongValue(n))
                  }
                case ColumnType.FLOAT | ColumnType.DOUBLE =>
                  ColString(colparam(2)).foreach { src =>
                    val d = NumericDouble(src)
                    if (colparam.head.getValue.nonEmpty && NumericDouble(DataValue.getStringValue(colparam.head.getValue)) > d)
                      colparam.head.setValue(DoubleValue(d))
                  }
              }
            case MAX =>
              colparam.head.typ match {
                case ColumnType.STRING =>
                  ColString(colparam(2)).foreach { src =>
                    if (colparam.head.getValue.nonEmpty && DataValue.getStringValue(colparam.head.getValue) < src)
                      colparam.head.setValue(StringValue(src))
                  }
                case ColumnType.INTEGER | ColumnType.LONG =>
                  ColString(colparam(2)).foreach { src =>
                    val n = NumericLong(src)
                    if (colparam.head.getValue.nonEmpty && NumericLong(DataValue.getStringValue(colparam.head.getValue)) < n)
                      colparam.head.setValue(LongValue(n))
                  }
                case ColumnType.FLOAT | ColumnType.DOUBLE =>
                  ColString(colparam(2)).foreach { src =>
                    val d = NumericDouble(src)
                    if (colparam.head.getValue.nonEmpty && NumericDouble(DataValue.getStringValue(colparam.head.getValue)) < d)
                      colparam.head.setValue(DoubleValue(d))
                  }
              }
          }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLBOUND", Option(e)), true)
      }
  }
}
