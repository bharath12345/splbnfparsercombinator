package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.utils.MatchUtils._

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColWhen extends Logger {
  private final lazy val logger = Logging(this)
}

class ColWhen(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColWhen._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLWHEN =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception(s"COLWHEN first parameter must be a column, l# $splline")
      }

      if (!colparam(1).isInstanceOf[RegexColumnParameter]) {
        throw new Exception(s"COLWHEN second parameter must be a pattern, l# $splline")
      }

      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      COS.cases match {
        case NOCASE =>
          logger.error(SM.mpspath, "WHEN/ELSE without CASE", true)

        case CASEWHEN =>
          def matchString() = {
            val column = getColumnForCOLUMN(colparam.head.column, COS)
            column.getValue match {
              case StringValue(s) => matchFound(colparam(1).regex, s)
              case _ => false
            }
          }

          if (matchString()) {
            COS.cases = CASETHEN
            logger.debug(SM.mpspath, s"COLWHEN Matched, col=${colparam.head.name}, " +
              s"Regex = " + colparam(1).regex + ", Param = " + colparam.head.getValue +
              s"COLWHEN Matched, COS.cases=${COS.cases} ")
          } else {
            logger.debug(SM.mpspath, s"COLWHEN didn't Match, Looking for subsequent COLWHEN's. " +
              s"Regex=${colparam(1).regex}, Param=EMPTY or false")
            COS.cases = CASEWHEN
          }

        case CASETHEN =>
          COS.cases = CASEEND
          logger.debug(SM.mpspath, s"CASETHEN Of COLWHEN got Called, cases=${COS.cases}")

        case _ =>
          logger.debug(s"CASE::COLWHEN, No match found, cases=${COS.cases}, l# $splline")
      }
  }
}