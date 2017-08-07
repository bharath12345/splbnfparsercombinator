package com.glassbeam.scalar.core

import com.glassbeam.scalar.core.parser.{CASES, Column}
import com.glassbeam.scalar.model.{Logger, StartupConfig}
import com.glassbeam.scalar.model.types.{Customer, Spltable}

import scala.collection.immutable.ListMap

/**
  * Created by bharadwaj on 06/08/17.
  */
package object colops extends Logger {

  // IMPORTANT - GET RID of this whole file when you merge with LCP

  val logger = Logging(this)

  def empty(s: SharedImmutables, c: ColOpSharables) = Unit

  class SharedImmutables extends Error {
    val splinstance = ""
    val ec: Customer = "".asInstanceOf[Customer]

    def numberOfTables: Int = 0

    lazy val mps = ("", "", "")
    lazy val (mfr, prod, sch) = mps
    lazy val mpspath = mfr + StartupConfig.filesep + prod + StartupConfig.filesep + sch

    def fatal: String => Unit = logger.info
    def error: String => Unit = logger.info
    def warning: String => Unit = logger.info

    def lineno = 0
  }


  class ColOpSharables {
    // variables used in ColOp
    var cols = new ListMap[String, Column]
    var cases = CASES.NOCASE
    var toConstrain = false
    var seenRowsplit = false
    var rowsplitIterlen = 0
    var row = 0

    protected var solrts: Long = 0L

    val table_name: Spltable = null
  }

  val COS = new ColOpSharables
  val SIM = new SharedImmutables
}
