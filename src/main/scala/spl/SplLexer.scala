package spl

import spl.NamespaceType.NamespaceType

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait SPL

case class TABLE(name: String, namespace: String, desc: Option[String]) extends SPL
case class NAMESPACE(name: String, desc: Option[String], nstype: Option[NamespaceType], isLock: Boolean, ref: Option[String],
                     isXml: Boolean, isJson: Boolean, isSolr: Boolean, maxLines: Option[Long]) extends SPL
case class COLUMN(name: String, aspect: Option[String], ddl: Option[String], attribs: String, as: Option[String],
                  align: Option[String], solrmap: Option[String], kafka: Boolean) extends SPL
case class ICON1() extends SPL // icon_r
case class ICON2() extends SPL // icon_nvpair_r
case class ICON3() extends SPL // icon_nv_unordered_r
case class ICON4() extends SPL // icon_xml_r
case class ICON5() extends SPL // icon_json_r
case class ICON6() extends SPL // icon_align_r
case class ICON7() extends SPL // multi_table_align_r
case class ICON8() extends SPL // icon_apache_r
case class ICON9() extends SPL // icon_csv_r
case class LINEGRAB() extends SPL
case class SETXMLNAMESPACE() extends SPL
case class ADDCONTEXT() extends SPL
case class MULTILINE() extends SPL
case class MULTILINE_BREAK_ON_UNMATCH() extends SPL
case class SKIP() extends SPL
case class OBJECT() extends SPL
case object EXIT extends SPL

trait RegexMatcher[T] extends RegexParsers {

  protected def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input): ParseResult[Regex.Match] = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      r findPrefixMatchOf source.subSequence(start, source.length) match {
        case Some(matched) =>
          Success(matched,
            in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }

  protected val regex: Regex

  protected def get(name: String, str2: String, str3: String, str4: String, str5: String, str6: String, str7: String, str8: String, str9: String): T

  def apply(): Parser[T] = {
    regexMatch(regex) ^^ { case m => get(m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6),
      m.group(7), m.group(8), m.group(9)) }
  }
}

object SplLexer extends RegexParsers {

  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f]+".r

  /*def table: Parser[TABLE] = {
    """^DEFINE\s+TABLE\s+([\w_\{\}]+)\s+NAMESPACE\s+([\w\.\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""".r ^^ { name => TABLE(name) }
  }*/

  def namespace: Namespace.Parser[NAMESPACE] = Namespace()

  object Namespace extends RegexMatcher[NAMESPACE] {
    def get(name: String, str2: String, str3: String, str4: String, str5: String, str6: String, str7: String, str8: String, str9: String) = {
      val desc = Option(str2)
      val nstype = Option(str3).map(x => NamespaceType.stringToEnum(x))
      val isLock = Option(str4).isEmpty
      val ref = Option(str5)
      val isXml = Option(str6).isEmpty
      val isJson = Option(str7).isEmpty
      val isSolr = Option(str8).isEmpty
      val maxlines = Option(str9).map(_.toLong)
      NAMESPACE(name, desc, nstype, isLock, ref, isXml, isJson, isSolr, maxlines)
    }

    val regex: Regex = ("""^DEFINE\s+NAMESPACE\s+([\w\._\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""" +
      """(TYPE\s+\w+|)\s*(LOCK|)\s*(REF\s+[\w_\.]+|)\s*(XML|JSON|)\s*(SOLR)?\s*(\d+)?""").r
  }

  /*def namespace: Parser[NAMESPACE] = {
    def get(name: String, str2: String, str3: String, str4: String, str5: String, str6: String, str7: String, str8: String, str9: String) = {
      val desc = Option(str2)
      val nstype = Option(str3).map(x => NamespaceType.stringToEnum(x))
      val isLock = Option(str4).isEmpty
      val ref = Option(str5)
      val isXml = Option(str6).isEmpty
      val isJson = Option(str7).isEmpty
      val isSolr = Option(str8).isEmpty
      val maxlines = Option(str9).map(_.toLong)
      NAMESPACE(name, desc, nstype, isLock, ref, isXml, isJson, isSolr, maxlines)
    }

    val regex: Regex = ("""^DEFINE\s+NAMESPACE\s+([\w\._\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""" +
      """(TYPE\s+\w+|)\s*(LOCK|)\s*(REF\s+[\w_\.]+|)\s*(XML|JSON|)\s*(SOLR)?\s*(\d+)?""").r

    regexMatch(regex) ^^ { case m => get(m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6),
      m.group(7), m.group(8), m.group(9)) }
  }*/
}
