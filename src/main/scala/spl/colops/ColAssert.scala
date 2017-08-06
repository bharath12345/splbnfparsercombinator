package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.{EmptyValue, Logger}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColAssert extends Logger {
  private final lazy val logger = Logging(this)
}

class ColAssert(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColAssert._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLASSERT =>
      val colerror =
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          SM.error(s"COLASSERT requires ONE column, l# $splline")
          true
        } else {
          false
        }
      if (colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      if (colparam.head.getValue.isEmpty || colparam.head.getValue.toString.isEmpty) {
        val msg = s"COLASSERT (${colparam.head.name}). splline = $splline"
        logger.error(SM.mpspath, msg, true)
      }
  }
}
