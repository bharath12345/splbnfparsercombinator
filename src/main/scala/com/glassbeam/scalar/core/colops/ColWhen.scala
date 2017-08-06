package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.CASES._
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}
import com.glassbeam.scalar.utils.MatchUtils._

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColWhen extends Logger {
  private final lazy val logger = Logging(this)
}

class ColWhen(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColWhen._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLWHEN =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        SM.error(s"COLWHEN first parameter must be a column, l# $splline")
        colerror = false
      }

      if (!colparam(1).isInstanceOf[RegexColumnParameter]) {
        SM.error(s"COLWHEN second parameter must be a pattern, l# $splline")
        colerror = false
      }

      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      COS.cases match {
        case NOCASE =>
          logger.error(SM.mpspath, "WHEN/ELSE without CASE", true)
          SM.warning(s"COLWHEN without CASE, l# $splline")

        case CASEWHEN =>
          def matchString() = {
            colparam.head.getValue match {
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