package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.core.parser.Ops._
import ColOp.ColumnParameter

import scala.collection.immutable.Vector

/**
  * Created by bharadwaj on 01/12/16.
  */
class ColBranch(colparam: Vector[ColumnParameter], op: String, param: String, splline: Int)
  extends ColOpFunction(colparam, op, param, splline) {

  def verify: PartialFunction[Ops, Unit => Unit] = {
    case COLBRANCH =>
      throw new Exception(s"COLBRANCH not yet implemented, l# $splline")
      PartialFunction.empty
  }
}