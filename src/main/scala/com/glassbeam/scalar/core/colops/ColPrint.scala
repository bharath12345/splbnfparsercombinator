package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{EmptyValue, Logger}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColPrint extends Logger {
  private final lazy val logger = Logging(this)
}

class ColPrint(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColPrint._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLPRINT =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLPRINT requires ONE column, l# $splline")
        colerror = true
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      val value: String = if(colparam.head.getValue.isEmpty) "EMPTY" else colparam.head.getValue.toString
      logger.debug(SM.mpspath, s"COLPRINT (${colparam.head.name}) = $value")
  }
}