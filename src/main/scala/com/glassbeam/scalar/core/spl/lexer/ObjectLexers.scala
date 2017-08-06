package com.glassbeam.scalar.core.spl.lexer

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object ObjectLexer extends RegexLexer {

  type T = OBJECT

  override protected val regex: Regex = """DEFINE\s+OBJECT\s+(\w+)""".r

  override protected def get(values: String*): OBJECT = OBJECT(values.head)
}

object LabelLexer extends RegexLexer {

  type T = LABEL

  override protected val regex: Regex = """LABEL\s+\'(.+?)\'""".r

  override protected def get(values: String*): LABEL = LABEL(values.head)
}

object KeyLexer extends RegexLexer {

  type T = KEY

  override protected val regex: Regex = """KEY\s+\((.+?)\)""".r

  override protected def get(values: String*): KEY = {
    val keys = values.head.split(",").view.map(x => x.trim).filter(_.nonEmpty).toSet
    KEY(keys)
  }
}

object ParentLexer extends RegexLexer {

  type T = PARENT

  override protected val regex: Regex = """PARENT\s+\((.*)\)""".r

  override protected def get(values: String*): PARENT = PARENT(values.toSet)
}