package spl.lexer

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object NamespaceMatcher extends RegexMatcher[NAMESPACE] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val desc = Option(value_list(1))
    val nstype = Option(value_list(2)).map(x => NamespaceType.stringToEnum(x))
    val isLock = Option(value_list(3)).isEmpty
    val ref = Option(value_list(4))
    val isXml = Option(value_list(5)).contains("XML")
    val isJson = Option(value_list(5)).contains("JSON")
    val isSolr = Option(value_list(6)).isEmpty
    val maxlines = Option(value_list(7)).map(_.toLong)
    NAMESPACE(name, desc, nstype, isLock, ref, isXml, isJson, isSolr, maxlines)
  }

  override protected val regex: Regex = ("""^DEFINE\s+NAMESPACE\s+([\w\._\{\}]+)\s*DESCRIPTION\s+('.+?'|)\s*""" +
    """TYPE\s+(\w+|)\s*(LOCK|)\s*(REF\s+[\w_\.]+|)\s*(XML|JSON|)\s*(SOLR)?\s*(\d+)?""").r
}

object BeginsWithMatcher extends RegexMatcher[BEGINS_WITH] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val regex = value_list.head
    BEGINS_WITH(regex.r)
  }

  override protected val regex: Regex = """^BEGINS\s+WITH\s+/(.+?)/$""".r
}

object EndsWithMatcher extends RegexMatcher[ENDS_WITH] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val regex = value_list.head
    ENDS_WITH(regex.r)
  }

  override protected val regex: Regex = """^ENDS\s+WITH\s+/(.+?)/$""".r
}

object FilePatternMatcher extends RegexMatcher[FILEPATTERN] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val regex = value_list.head
    FILEPATTERN(regex.r)
  }

  override protected val regex: Regex = """^FILEPATTERN\s+/(.+?)/$""".r
}
