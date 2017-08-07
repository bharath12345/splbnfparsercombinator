package com.glassbeam.scalar.core.parser

import com.glassbeam.scalar.core.colops.SharedImmutables
import com.glassbeam.scalar.core.spl.lexer.COLUMN
import com.glassbeam.scalar.model.{DataValue, EmptyValue}
import com.glassbeam.scalar.model.types.Spltable

case class ActorRef()

class Column(private val table_name: Spltable, private val position: Int, val column: COLUMN,
             private val SIM: SharedImmutables, private val supervisor: ActorRef, var persist: Boolean = true,
             private val splline: Int = 0) {


  def setValue(value: DataValue) = Unit

  def setPreviousValue(value: DataValue) = Unit

  def getValue: DataValue = EmptyValue

  def getPreviousValue: DataValue = EmptyValue

  def getDDL(ddl: String): Option[(String, Int, String, String)] = None
}
