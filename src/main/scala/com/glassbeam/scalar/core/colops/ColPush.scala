package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import com.glassbeam.scalar.core.colops.ColOp.{ColColumnParameter, ColumnParameter}

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColPush(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[Ops, (SharedImmutables, ColOpSharables) => Unit] = {
    case COLPUSH =>
      for (p <- colparam)
        if (!p.isInstanceOf[ColColumnParameter])
          throw new Exception(s"COLPUSH only takes COLUMN parameters, l# $splline")
      empty
  }
}