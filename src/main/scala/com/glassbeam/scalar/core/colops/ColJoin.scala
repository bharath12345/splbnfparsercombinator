package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.StringValue
import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColJoin(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLJOIN(destCol, [literal|srccolN])
    case COLJOIN =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLJOIN first parameter must be a COLUMN, l# $splline")
      }
      if (colparam.size < 3) {
        throw new Exception(s"COLJOIN must have at least three parameters, l# $splline")
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      var value: String = ""
      for {x <- colparam.tail; y <- ColString(x)}
        value += y
      colparam.head.setValue(StringValue(value))
  }
}