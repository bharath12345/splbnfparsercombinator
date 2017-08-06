package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import ColOp.ColumnParameter

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class Constrain(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case CONSTRAIN =>
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      for (c <- COS.cols; if c._2.persist && !COS.toConstrain)
        COS.toConstrain = c._2.constrain
  }
}