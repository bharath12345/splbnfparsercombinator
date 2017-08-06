package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model.StringValue
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColJoin(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLJOIN(destCol, [literal|srccolN])
    case COLJOIN =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLJOIN first parameter must be a COLUMN, l# $splline")
        colerror = true
      }
      if (colparam.size < 3) {
        SM.error(s"COLJOIN must have at least three parameters, l# $splline")
        colerror = true
      }
      if (colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      var value: String = ""
      for {x <- colparam.tail; y <- ColString(x)}
        value += y
      colparam.head.setValue(StringValue(value))
  }
}