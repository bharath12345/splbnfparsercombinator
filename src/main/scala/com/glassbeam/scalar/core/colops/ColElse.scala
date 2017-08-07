package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.ColumnParameter
import com.glassbeam.scalar.model.Logger

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColElse extends Logger {
  private final lazy val logger = Logging(this)
}

class ColElse(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColElse._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLELSE =>
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      COS.cases match {
        case NOCASE =>
          logger.error(SM.mpspath, "WHEN/ELSE without CASE", true)
          SM.warning(s"(COLELSE) without CASE, l# $splline")

        case CASEWHEN =>
          COS.cases = CASETHEN
          logger.debug(SM.mpspath, s"Default(COLELSE) got invoked, setting cases=${COS.cases}")

        case CASETHEN =>
          COS.cases = CASEEND
          logger.debug(SM.mpspath, s"CASETHEN Of COLWHEN got Called, cases=${COS.cases}")

        case _ =>
          logger.debug(s"CASE::COLWHEN, No match found, cases=${COS.cases}, l# $splline")
      }
  }
}