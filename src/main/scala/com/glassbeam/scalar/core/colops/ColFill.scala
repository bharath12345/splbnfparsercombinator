package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.EmptyValue

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColFill(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLFILL(col1 [, colN])
    case COLFILL =>
      for (p <- colparam)
        if (!p.isInstanceOf[ColColumnParameter]) {
          throw new Exception(s"COLFILL only takes COLUMN parameters, l# $splline")
        }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      for (o <- colparam)
        if (o.getValue.isEmpty)
          o.setValue(o.getPrev)
        else o.setPrev(o.getValue)
  }
}
