package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColDrop(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLDROP(col1 [, coln])
    case COLDROP =>
      for (p <- colparam)
        if (!p.isInstanceOf[ColColumnParameter]) {
          throw new Exception(s"COLDROP only takes COLUMN parameters, l# $splline")
        }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      for (o <- colparam) {
        val column = getColumnForCOLUMN(o.column, COS)
        column.persist = false
      }
  }

}
