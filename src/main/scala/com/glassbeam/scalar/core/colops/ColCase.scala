package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.ColumnParameter
import com.glassbeam.scalar.model.Logger

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColCase extends Logger {
  private final lazy val logger = Logging(this)
}

class ColCase(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColCase._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLCASE => exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      if (COS.cases > NOCASE) {
        logger.warning(s"Embedded (COLCASE) not allowed , l# $splline")
      } else {
        COS.cases = CASEWHEN
        logger.debug(SM.mpspath, s"COLCASE got Called, COS.cases=${COS.cases}")
      }
  }
}
