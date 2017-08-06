package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.colops.ColOp.ColumnParameter
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.model.Logger

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColEnd extends Logger {
  private final lazy val logger = Logging(this)
}

class ColEnd(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColEnd._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLEND =>
      exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      if (COS.cases == CASETHEN || COS.cases == CASEEND) {
        COS.cases = NOCASE
        logger.debug(SM.mpspath, s"COLEND got Called, COS.cases=${COS.cases}")
      } else{
        COS.cases = NOCASE
        SM.warning(s"COLEND without CASE, l# $splline")
      }

  }
}