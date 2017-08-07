package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.{EmptyValue, Logger}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColAssert extends Logger {
  private final lazy val logger = Logging(this)
}

class ColAssert(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColAssert._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLASSERT =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLASSERT requires ONE column, l# $splline")
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      val column = getColumnForCOLUMN(colparam.head.column, COS)
      if (column.getValue.isEmpty || column.getValue.toString.isEmpty) {
        val msg = s"COLASSERT (${colparam.head.name}). splline = $splline"
        logger.error(SM.mpspath, msg, true)
      }
  }
}
