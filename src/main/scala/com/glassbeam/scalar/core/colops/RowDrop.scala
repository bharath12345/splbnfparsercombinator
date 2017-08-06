package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger}
import com.glassbeam.scalar.core.parser.Funcs._
import com.glassbeam.scalar.core.parser.Ops._
import ColOp.{ColColumnParameter, ColumnParameter, LongColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by pallab on 12/27/16.
  */

object RowDrop extends Logger {
  private final lazy val logger = Logging(this)
}

class RowDrop(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import RowDrop._

  var durationInDays : Long = 1825 // 5 years

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case ROWDROP =>
      var func = InvalidFunc
      try {
        func = colparam(0).func
      } catch {
        case e: java.lang.ClassCastException =>
          throw new Exception(s"ROWDROP Function does not exist, l# $splline")
      }
      func match {
        case DISCARDOLDTIME =>
          if (colparam.size != 4) {
            throw new Exception(s"DISCARDOLDTIME must have  four parameters, l# $splline")
          }
          if (!colparam(1).isInstanceOf[ColColumnParameter] || !colparam(2).isInstanceOf[ColColumnParameter]) {
            throw new Exception(s"DISCARDOLDTIME must have param1 &2 as Column, l# $splline")
          }
          colparam(3) match {
            case LongColumnParameter(value) =>
              durationInDays = value.value
            case _ =>
              throw new Exception(s"DISCARDOLDTIME param 3 must be Long, l# $splline")
          }
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      val func = colparam(0).func
      logger.debug(SM.mpspath, "ROWDROP(" + splline + ") function Value = " + func)
      func match {
        case DISCARDOLDTIME =>
          logger.debug(SM.mpspath, s"DISCARDOLDTIME($splline) input=${ColString(colparam(1))}, ${ColString(colparam(2))}, ${colparam(3)}")
          try {
            if(colparam(1).getValue.isEmpty || colparam(2).getValue.isEmpty) {
              logger.error(SM.mpspath, s"DISCARDOLDTIME on empty value. splline = $splline", true)
            } else {
              val t_lower = DataValue.getLongValue(colparam(1).getValue)
              val t_upper = DataValue.getLongValue(colparam(2).getValue)
              //86400000 is millis in a day
              val duration: Long = durationInDays * 86400000L
              logger.debug(SM.mpspath, s"ROWDROP ${(t_upper - t_lower) > duration} / t_lower : $t_lower / t_upper: $t_upper " +
                s"/ duration : $duration , $durationInDays / diff :${(t_upper - t_lower)}")
              if ((t_upper - t_lower) > duration) COS.toConstrain = true
            }
          } catch {
            case NonFatal(e) =>
              logger.error(e, SM.mpspath, s"DISCARDOLDTIME ${ColString(colparam(1))}, ${ColString(colparam(2))}, ${colparam(3)} error : ${e.getMessage}", true)
          }
      }
  }
}