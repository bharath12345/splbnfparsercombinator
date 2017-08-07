package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.{Logger, StringValue}
import com.glassbeam.scalar.core.parser.ColumnOps._
import ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector
import scala.util.control.NonFatal

/**
  * Created by bharadwaj on 01/12/16.
  */
object ColLookupByName extends Logger {
  private final lazy val logger = Logging(this)
}

class ColLookupByName(colparam: Vector[ColumnParameter], op: ColumnOps, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  import ColLookupByName._

  def verify: PartialFunction[ColumnOps, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLLOOKUPBYNAME =>
      if (!colparam.head.isInstanceOf[ColColumnParameter]) {
        throw new Exception("the lookup target is not defined as a column")
      } else {
        logger.info("COL-LOOKUP-BY-NAME definition found")
      }
      exec
  }

  private def exec: (SharedImmutables, ColOpSharables) => Unit = {
    (SM: SharedImmutables, COS: ColOpSharables) =>
      try {
        val lookupColumn = ColLookUpCache.lookupValueOfKey(SM.mfr + "/" + SM.prod + "/" + SM.sch, colparam(1).toString())
        val column = getColumnForCOLUMN(colparam.head.column, COS)
        column.setValue(StringValue(lookupColumn.get))
      } catch {
        case NonFatal(e) =>
          logger.error(e, SM.mpspath, warningString("COLLOOKUPBYNAME", Option(e)), true)
      }
  }
}