package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.EmptyValue

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColFill(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLFILL(col1 [, colN])
    case COLFILL =>
      var colerror = false
      for (p <- colparam)
        if (!p.isInstanceOf[ColColumnParameter]) {
          SM.error(s"COLFILL only takes COLUMN parameters, l# $splline")
          colerror = true
        }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      for (o <- colparam)
        if (o.getValue.isEmpty)
          o.setValue(o.getPrev)
        else o.setPrev(o.getValue)
  }
}
