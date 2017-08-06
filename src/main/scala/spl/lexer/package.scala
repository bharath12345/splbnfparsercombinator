package spl

import spl.colops.Ops.Ops
import spl.lexer.Icons.Icons
import spl.lexer.NamespaceType.NamespaceType

import scala.util.matching.Regex
import scala.util.parsing.input.Positional

/**
  * Created by bharadwaj on 30/07/17.
  */
package object lexer {

  object NamespaceType extends Enumeration {
    type NamespaceType = Value
    val SECTION, EVENT, UNPARSED, TRASH, STAT, SESSION, SESSION_ATTR, GOLDEN, ALERT, LOGVAULT = Value

    val SectionString = SECTION.toString
    val EventString = EVENT.toString
    val UnparsedString = UNPARSED.toString
    val TrashString = TRASH.toString
    val StatString = STAT.toString
    val SessionString = SESSION.toString
    val SessionAttrString = SESSION_ATTR.toString
    val GoldenString = GOLDEN.toString
    val AlertString = ALERT.toString
    val LogvaultString = LOGVAULT.toString

    def stringToEnum(typ: String): NamespaceType = {
      val enum = this.withName(typ)
      enum match {
        case SECTION | EVENT | UNPARSED | TRASH | STAT | SESSION | SESSION_ATTR | GOLDEN | ALERT | LOGVAULT => enum
        case _ => throw new Exception(s"Unknown namespace type = $typ")
      }
    }
  }

  object Icons extends Enumeration {
    type Icons = Value
    val LIST_BASIC, NVPAIR_BASIC, NVPAIR_UNORDERED, ALIGNED_BASIC, XML_BASIC, JSON, CSV_NOHEADER, SYSLOG, APACHE, CSV_WITHHEADER = Value
  }

  def emptyIfNone(title: String, os: Option[String]): String = if(os.isEmpty) "" else s"$title = ${os.get},"

  sealed trait SplToken

  sealed trait SplSpecialToken extends SplToken
  case class SPL_ERROR(error: String, linenum: Int) extends SplSpecialToken
  case object EXIT extends SplSpecialToken

  sealed trait SplNamespaceToken extends SplToken
  case class NAMESPACE(name: String, desc: Option[String], nstype: Option[NamespaceType], isLock: Boolean, ref: Option[String],
                       isXml: Boolean, isJson: Boolean, isSolr: Boolean, maxLines: Option[Long]) extends SplNamespaceToken {
    override def toString: String = {
      s"NAMESPACE($name, ${emptyIfNone("desc", desc)} ${emptyIfNone("nstype", nstype.map(_.toString))} lock = $isLock, " +
        s"${emptyIfNone("ref", ref)} isXml = $isXml, isJson = $isJson, ${emptyIfNone("maxlines", maxLines.map(_.toString))} isSolr = $isSolr)"
    }
  }
  case class BEGINS_WITH(regex: Regex) extends SplNamespaceToken
  case class ENDS_WITH(regex: Regex) extends SplNamespaceToken
  case class FILEPATTERN(regex: Regex) extends SplNamespaceToken
  case class CONTEXT(regex: Regex) extends SplNamespaceToken
  case class AS(colnames: List[String]) extends SplNamespaceToken
  case class BUNDLETYPE(btype: String) extends SplNamespaceToken

  sealed trait SplTableToken extends SplToken
  case class TABLE(name: String, namespace: String, desc: Option[String]) extends SplTableToken
  case class COLUMN(table_name: String, column_name: String, aspect: Option[String], ddl: Option[String], attribs: String, as: Option[String],
                    align: Option[String], solrmap: Option[String], kafka: Boolean) extends SplTableToken {
    override def toString: String = {
      s"COLUMN($column_name, ${emptyIfNone("aspect", aspect)} ${emptyIfNone("ddl", ddl)} attribs = $attribs, ${emptyIfNone("as", as)} " +
        s"${emptyIfNone("align", align)} ${emptyIfNone("solrmap", solrmap)} kafka = $kafka)"
    }
  }
  case class LINEGRAB(regex: Regex) extends SplTableToken
  case class SETXMLNAMESPACE(urls: Array[String]) extends SplTableToken
  case class ADDCONTEXT(params: String) extends SplTableToken
  case class MULTILINE(not: Boolean, pat: Regex, sub: String) extends SplTableToken
  case class MULTILINE_BREAK_ON_UNMATCH(pat: Regex, sub: String) extends SplTableToken
  case class SKIP(n: Int) extends SplTableToken

  sealed trait ICON extends SplTableToken { val icon: Icons }
  case class ICON1(icon: Icons) extends ICON // icon_r
  case class ICON2(icon: Icons, separator: Regex, delimiter: Regex) extends ICON // icon_nvpair_r
  case class ICON3(icon: Icons, key: Regex) extends ICON // icon_nv_unordered_r
  case class ICON4(icon: Icons, xpath: String) extends ICON // icon_xml_r
  case class ICON5(icon: Icons, jpath: String) extends ICON // icon_json_r
  case class ICON6(icon: Icons, horSep: String, verSep: String, multilineKey: String) extends ICON // icon_align_r
  case class ICON7(icon: Icons) extends ICON // multi_table_align_r
  case class ICON8(icon: Icons, logformat: String) extends ICON // icon_apache_r
  case class ICON9(icon: Icons, csvType: String, sep: String) extends ICON // icon_csv_r

  /*
    1. Each colop has a verify function which returns a exec function. verify stores mutable state, that is used by exec
    2. A colop takes a vector of ColumnParameter as input. ColumnParameter is a set of types: String, Numeric, Regex, Column and ColumnFunction

    What to do:
    1. Dont bring ColOpFunction into this project
    2. Table has a list of ColOpTrait's
    3. ColOpTrait has to be brought in but not with SharedImmutables and ColOpSharables
    4. Run the verification after creating of a ColOp object. And if the verify fails then throw exception

    The ugly part is each ColOp creates all the operation objects - remove this by switching on columnOperation. Create only one object
   */

  case class COLOP(colop: Ops, coloptext: String) extends SplTableToken
  case class ROWOP(rowop: String, rowoptext: String) extends SplTableToken
  case class COPCASEOP(op: Ops) extends SplTableToken

  sealed trait SplObjectToken extends SplToken
  case class OBJECT(name: String) extends SplObjectToken
  case class LABEL(label: String) extends SplObjectToken
  case class KEY(keys: Set[String]) extends SplObjectToken
  case class PARENT(parents: Set[String]) extends SplObjectToken

  case class SplTokenSuperType(splToken: SplToken, linenum: Int) {
    override def toString: String = s"[$splToken, $linenum]"
  }

}
