package com.glassbeam.scalar.core.parser.colops

import com.glassbeam.scalar.model.{EmptyValue, Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import com.glassbeam.scalar.core.parser.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColLookupByPosition extends Logger {
  private final lazy val logger = Logging(this)
}

class ColLookupByPosition(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColLookupByPosition._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLLOOKUPBYPOSITION =>
      var colerror = false
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        SM.error("the lookup target is not defined as a column")
        colerror = true
      } else
        logger.info(SM.mpspath, "COL-LOOKUP-BY-NAME definition found")

      if(colerror) PartialFunction.empty
      else exec
  }

  private def exec: Unit => Unit = {
    Unit =>
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
            colparam.head.column.setValue(StringValue(lookupColumn.get))
          }
        }
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLLOOKUPBYPOSITION", Option(e)), true)
      }
  }
}