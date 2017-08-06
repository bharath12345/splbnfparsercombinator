package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.{EmptyValue, Logger}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColAssert extends Logger {
  private final lazy val logger = Logging(this)
}

class ColAssert(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColAssert._

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLASSERT =>
      val colerror =
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          throw new Exception(s"COLASSERT requires ONE column, l# $splline")
          true
        } else {
          false
        }
      if (colerror) empty
      else exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      if (colparam.head.getValue.isEmpty || colparam.head.getValue.toString.isEmpty) {
        val msg = s"COLASSERT (${colparam.head.name}). splline = $splline"
        logger.error(SM.mpspath, msg, true)
      }
  }
}
