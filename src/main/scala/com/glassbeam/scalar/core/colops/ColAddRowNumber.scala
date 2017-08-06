package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{LongValue, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, StringColumnParameter}
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}
import spl.colops.Ops.Ops

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColAddRowNumber(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case ADD_ROW_NUMBER =>
      val colerror =
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          SM.error(s"ADD_ROW_NUMBER requires ONE column, l# $splline")
          true
        } else {
          false
        }

      if (colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      colparam.tail.last match {
        case StringColumnParameter(StringValue("$row")) =>
          colparam.head.setValue(LongValue(COS.row))
        case _ =>
          SM.warning(s"ADD_ROW_NUMBER($splline) NOT SUPPORTED")
      }
  }
}
