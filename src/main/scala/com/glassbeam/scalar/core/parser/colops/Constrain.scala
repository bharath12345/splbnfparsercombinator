package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.ColumnParameter

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class Constrain(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case CONSTRAIN =>
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      for (c <- COS.dataColumns; if c._2.persist && !COS.toConstrain)
        COS.toConstrain = c._2.constrain
  }
}