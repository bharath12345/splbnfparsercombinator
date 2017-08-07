package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.model.{EmptyValue, Logger}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCopy extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCopy(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    // COLCOPY(srcCol|literal, destCol1 [, destColN])
    case COLCOPY => // src-col, target-col, ...
      if (colparam.size < 2) {
        throw new Exception(s"COLCOPY must have more than one parameter, l# $splline")
      } else {
        for (c <- colparam.tail)
          if (!c.isInstanceOf[ColColumnParameter]) {
            throw new Exception(s"COLCOPY subsequent parameters must be COLUMN, l# $splline")
          }
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      for {
        c <- colparam.tail
        if colparam.head.getValue.nonEmpty
      } {
        c.column.setValue(colparam.head.getValue)
        //logger.debug(SM.mpspath, s"COLCOPY: table = ${COS.table_name}, col = ${c.name}, value = ${c.getValue}")
      }
  }
}
