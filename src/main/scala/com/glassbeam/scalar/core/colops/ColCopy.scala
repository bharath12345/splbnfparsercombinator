package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import ColOp.{ColColumnParameter, ColumnParameter}
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.model.{EmptyValue, Logger}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCopy extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCopy(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    // COLCOPY(srcCol|literal, destCol1 [, destColN])
    case COLCOPY => // src-col, target-col, ...
      var colerror = false
      if (colparam.size < 2) {
        SM.error(s"COLCOPY must have more than one parameter, l# $splline")
        colerror = true
      } else {
        for (c <- colparam.tail)
          if (!c.isInstanceOf[ColColumnParameter]) {
            SM.error(s"COLCOPY subsequent parameters must be COLUMN, l# $splline")
            colerror = true
          }
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      for {
        c <- colparam.tail
        if colparam.head.getValue.nonEmpty
      } {
        c.column.setValue(colparam.head.getValue)
        //logger.debug(SM.mpspath, s"COLCOPY: table = ${COS.table_name}, col = ${c.name}, value = ${c.getValue}")
      }
  }
}
