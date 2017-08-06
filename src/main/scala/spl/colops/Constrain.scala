package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.ColumnParameter

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class Constrain(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case CONSTRAIN =>
      exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      for (c <- COS.cols; if c._2.persist && !COS.toConstrain)
        COS.toConstrain = c._2.constrain
  }
}