package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.ColumnParameter
import com.glassbeam.scalar.model.Logger

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColEnd extends Logger {
  private final lazy val logger = Logging(this)
}

class ColEnd(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColEnd._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLEND =>
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      if (COS.cases == CASETHEN || COS.cases == CASEEND) {
        COS.cases = NOCASE
        logger.debug(SM.mpspath, s"COLEND got Called, COS.cases=${COS.cases}")
      } else{
        COS.cases = NOCASE
        SM.warning(s"COLEND without CASE, l# $splline")
      }

  }
}