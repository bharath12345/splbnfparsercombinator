package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import ColOp.ColumnParameter

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColFile(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLFILE =>
      SM.error(s"COLFILE not yet implemented, l# $splline")
      PartialFunction.empty
  }
}