package com.glassbeam.scalar.core.spl.lexer

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object NamespaceLexer extends RegexLexer {

  type T = NAMESPACE

  private def getValueFromDef(dy: String, start: String, default: String): String = {
    if (dy != null && dy.nonEmpty && dy.startsWith(start)) dy.split("\\s+")(1).trim
    else default
  }

  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val desc = Option(value_list(1))
    val nstype = Option(value_list(2)).map(x => NamespaceType.stringToEnum(getValueFromDef(x, "TYPE", NamespaceType.SectionString)))
    val isLock = Option(value_list(3)).isEmpty
    val ref = Option(value_list(4))
    val isXml = Option(value_list(5)).contains("XML")
    val isJson = Option(value_list(5)).contains("JSON")
    val isSolr = Option(value_list(6)).isEmpty
    val maxlines = Option(value_list(7)).map(_.toLong)
    NAMESPACE(name, desc, nstype, isLock, ref, isXml, isJson, isSolr, maxlines)
  }

  override protected val regex: Regex = ("""^DEFINE\s+NAMESPACE\s+([\w\._\{\}]+)\s*DESCRIPTION\s+('.+?'|)\s*""" +
    """(TYPE\s+\w+|)\s*(LOCK|)\s*(REF\s+[\w_\.]+|)\s*(XML|JSON|)\s*(SOLR)?\s*(\d+)?""").r
}

object BeginsWithLexer extends RegexLexer {

  type T = BEGINS_WITH

  override protected def get(values: String*) = BEGINS_WITH(values.head.r)

  override protected val regex: Regex = """^BEGINS\s+WITH\s+/(.+?)/$""".r
}

object EndsWithLexer extends RegexLexer {

  type T = ENDS_WITH

  override protected def get(values: String*) = ENDS_WITH(values.head.r)

  override protected val regex: Regex = """^ENDS\s+WITH\s+/(.+?)/$""".r
}

object FilePatternLexer extends RegexLexer {

  type T = FILEPATTERN

  override protected def get(values: String*) = FILEPATTERN(values.head.r)

  override protected val regex: Regex = """^FILEPATTERN\s+/(.+?)/$""".r
}

object ContextLexer extends RegexLexer {

  type T = CONTEXT

  override protected val regex: Regex = """^CONTEXT\s+/(.+?)/""".r

  override protected def get(values: String*): CONTEXT = CONTEXT(values.head.r)
}

object AsLexer extends RegexLexer {

  override type T = AS

  override protected val regex: Regex = """AS\s+([\w_ ,]+)""".r

  override protected def get(values: String*): AS = AS(values.head.split(",").map(_.trim).filter(_.nonEmpty).toList)
}

object BundleType extends RegexLexer {
  override type T = BUNDLETYPE

  override protected val regex: Regex = """^BUNDLETYPE\s+'(.+?)'$""".r

  override protected def get(values: String*): BUNDLETYPE = BUNDLETYPE(values.head)
}
