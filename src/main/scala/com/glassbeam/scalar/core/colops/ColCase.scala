package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.colops.ColOp.ColumnParameter
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.colops.ColOp.ColumnParameter
import com.glassbeam.scalar.model.Logger

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCase extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCase(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColCase._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLCASE =>
      exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      if (COS.cases > NOCASE) {
        SM.warning(s"Embedded (COLCASE) not allowed , l# $splline")
      } else {
        COS.cases = CASEWHEN
        logger.debug(SM.mpspath, s"COLCASE got Called, COS.cases=${COS.cases}")
      }
  }
}
