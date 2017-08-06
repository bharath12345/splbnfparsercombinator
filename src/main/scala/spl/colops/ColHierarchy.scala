package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter, RegexColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal
import scala.util.matching.Regex

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColHierarchy extends Logger {
  private final lazy val logger = Logging(this)
}

class ColHierarchy(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColHierarchy._

  private var hArray: Array[String] = null

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLHIERARCHY =>
      var colerror = false
      if (colparam.size < 4) {
        SM.error(s"COLHIERARCHY must have at least four parameters, l# $splline")
        colerror = true
      } else {
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          SM.error(s"COLHIERARCHY first parameter must be COLUMN, l# $splline")
          colerror = true
        }
        if (!colparam(2).isInstanceOf[RegexColumnParameter]) {
          SM.error(s"COLHIERARCHY third parameter must be a Regex, l# $splline")
          colerror = true
        }
      }
      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
      if(colparam.head.getValue.isEmpty) {
        logger.error(SM.mpspath, s"colhierarchy source is empty. splline = $splline", true)
      } else {
        val src = DataValue.getStringValue(colparam.head.getValue)
        val levels = colparam(1).toString.toInt
        val rxs = new Array[Regex](levels)
        for (i <- 0 until levels) rxs(i) = colparam(i + 2).regex
        logger.debug(SM.mpspath, "COLHIERARCHY(" + splline + "): src=" + src + " levels=" + levels + " rxs=" + rxs)

        def errr(reg: Regex, i: Int): String = {
          s"[regex = ${reg.toString()}] [index = $i] [line = $src]"
        }

        def hMatch(reg: Regex, i: Int): Boolean = {
          try {
            reg findFirstMatchIn src match {
              case Some(m) =>
                val cdl = colparam.drop(2 + levels)
                cdl(i).column.setValue(StringValue(m.group(1)))
                logger.debug(SM.mpspath, s"COLHIERARCHY MATCH OK: " + errr(reg, i))
                i match {
                  case 0 =>
                    hArray = new Array[String](levels)
                    hArray(i) = m.group(1)
                  case j if 1 until (levels - 2) contains j =>
                    hArray(i) = m.group(1)
                    for (k <- 0 until i)
                      cdl(k).column.setValue(StringValue(hArray(k)))
                  case levelsless =>
                    for (k <- 0 until i)
                      cdl(k).column.setValue(StringValue(hArray(k)))
                }
                true
              case None =>
                logger.debug(SM.mpspath, s"COLHIERARCHY MATCH ERROR: " + errr(reg, i))
                false
            }

          } catch {
            case NonFatal(e) =>
              logger.error(e, SM.mpspath, warningString("COLHIERARCHY", Option(e)), true)
              false
          }
        }
        var bfound = false
        for {i <- 0 until levels; if (!bfound)} {
          if (hMatch(rxs(i), i))
            bfound = true
        }
        if (!bfound) logger.debug(SM.mpspath, s"all hierarchical regex failed for line $src")
      }
  }
}