package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model.{DataValue, EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.ColumnOps._
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

class ColHierarchy(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColHierarchy._

  private var hArray: Array[String] = null

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLHIERARCHY =>
      if (colparam.size < 4) {
        throw new Exception(s"COLHIERARCHY must have at least four parameters, l# $splline")
      } else {
        if (!colparam.head.isInstanceOf[ColColumnParameter]) {
          throw new Exception(s"COLHIERARCHY first parameter must be COLUMN, l# $splline")
        }
        if (!colparam(2).isInstanceOf[RegexColumnParameter]) {
          throw new Exception(s"COLHIERARCHY third parameter must be a Regex, l# $splline")
        }
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
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
                val column = getColumnForCOLUMN(cdl(i).column, COS)
                column.setValue(StringValue(m.group(1)))
                logger.debug(SM.mpspath, s"COLHIERARCHY MATCH OK: " + errr(reg, i))
                i match {
                  case 0 =>
                    hArray = new Array[String](levels)
                    hArray(i) = m.group(1)
                  case j if 1 until (levels - 2) contains j =>
                    hArray(i) = m.group(1)
                    for (k <- 0 until i) {
                      val columnk = getColumnForCOLUMN(cdl(k).column, COS)
                      columnk.setValue(StringValue(hArray(k)))
                    }
                  case levelsless =>
                    for (k <- 0 until i) {
                      val columnk = getColumnForCOLUMN(cdl(k).column, COS)
                      columnk.setValue(StringValue(hArray(k)))
                    }
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