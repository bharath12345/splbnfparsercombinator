package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{LongValue, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, StringColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColAddRowNumber(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case ADD_ROW_NUMBER =>
      val colerror =
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          SIM.error(s"ADD_ROW_NUMBER requires ONE column, l# $splline")
          true
        } else {
          false
        }

      if (colerror) empty
      else exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      colparam.tail.last match {
        case StringColumnParameter(StringValue("$row")) =>
          colparam.head.setValue(LongValue(COS.row))
        case _ =>
          SM.warning(s"ADD_ROW_NUMBER($splline) NOT SUPPORTED")
      }
  }
}
