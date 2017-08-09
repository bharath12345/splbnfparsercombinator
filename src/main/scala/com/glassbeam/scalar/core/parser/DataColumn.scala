package com.glassbeam.scalar.core.parser

import com.glassbeam.scalar.core.parser.colops.SharedImmutables
import com.glassbeam.scalar.core.parser.spl.lexer.COLUMN
import com.glassbeam.scalar.model.ColumnType.ColumnType
import com.glassbeam.scalar.model.{ColumnType, DataValue, EmptyValue}
import com.glassbeam.scalar.model.types.Spltable

case class ActorRef()

class DataColumn(private val table_name: Spltable, private val position: Int, val column: COLUMN,
                 private val SIM: SharedImmutables, private val supervisor: ActorRef, var persist: Boolean = true,
                 private val splline: Int = 0) {

  var sess_count = 0
  val ddl = getDDL(column.ddl.get)
  val typ: ColumnType = ColumnType.get(column.column_name, ddl, splline)
  var len: Int = 0


  def constrain: Boolean = false

  def getDDL(ddl: String): Option[(String, Int, String, String)] = None

  def setValue(value: DataValue) = Unit

  def setPreviousValue(value: DataValue) = Unit

  def getValue: DataValue = EmptyValue

  def getPreviousValue: DataValue = EmptyValue
}
