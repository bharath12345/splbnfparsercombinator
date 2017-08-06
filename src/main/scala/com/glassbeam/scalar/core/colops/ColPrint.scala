package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{EmptyValue, Logger}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColPrint extends Logger {
  private final lazy val logger = Logging(this)
}

class ColPrint(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColPrint._

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLPRINT =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLPRINT requires ONE column, l# $splline")
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      val value: String = if(colparam.head.getValue.isEmpty) "EMPTY" else colparam.head.getValue.toString
      logger.debug(SM.mpspath, s"COLPRINT (${colparam.head.name}) = $value")
  }
}