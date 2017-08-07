package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.ColumnOps._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColLookupByPosition extends Logger {
  private final lazy val logger = Logging(this)
}

class ColLookupByPosition(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColLookupByPosition._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLLOOKUPBYPOSITION =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception("the lookup target is not defined as a column")
      } else
        logger.info("COL-LOOKUP-BY-NAME definition found")

      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      try {
        if(colparam(1).getValue.isEmpty) {
          logger.error(SM.mpspath, s"COLLOOKUPBYPOSITION on empty value. splline = $splline", true)
        } else {
          val position = Integer.parseInt(colparam(2).toString())
          val seekKey = colparam(1).getValue.toString()
          if (seekKey.length() > position) {
            val lookupColumn = ColLookUpCache.lookupValueOfKey(
              SM.mfr + "/" + SM.prod + "/" + SM.sch,
              colparam(1).toString().toCharArray()(position) + "", position)
            val column = getColumnForCOLUMN(colparam.head.column, COS)
            column.setValue(StringValue(lookupColumn.get))
          }
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLLOOKUPBYPOSITION", Option(e)), true)
      }
  }
}