package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.parser.{ColOpSharables, SharedImmutables, SharedMutables}
import ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColLookupByName extends Logger {
  private final lazy val logger = Logging(this)
}

class ColLookupByName(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int, SM: SharedImmutables, COS: ColOpSharables)
  extends ColOpFunction(colparam, op, param, splline, SM, COS) {

  import ColLookupByName._

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLLOOKUPBYNAME =>
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
        val lookupColumn = ColLookUpCache.lookupValueOfKey(SM.mfr + "/" + SM.prod + "/" + SM.sch, colparam(1).toString())
        colparam.head.column.setValue(StringValue(lookupColumn.get))
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLLOOKUPBYNAME", Option(e)), true)
      }
  }
}