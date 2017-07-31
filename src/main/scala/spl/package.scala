import spl.Icons.Icons
import spl.NamespaceType.NamespaceType

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 30/07/17.
  */
package object spl {

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

  sealed trait SPL

  case class SPL_ERROR(error: String) extends SPL
  case object EXIT extends SPL

  case class NAMESPACE(name: String, desc: Option[String], nstype: Option[NamespaceType], isLock: Boolean, ref: Option[String],
                       isXml: Boolean, isJson: Boolean, isSolr: Boolean, maxLines: Option[Long]) extends SPL
  case class COLUMN(name: String, aspect: Option[String], ddl: Option[String], attribs: String, as: Option[String],
                    align: Option[String], solrmap: Option[String], kafka: Boolean) extends SPL
  case class BEGINS_WITH(regex: Regex) extends SPL
  case class ENDS_WITH(regex: Regex) extends SPL
  case class FILEPATTERN(regex: Regex) extends SPL

  case class TABLE(name: String, namespace: String, desc: Option[String]) extends SPL
  case class LINEGRAB() extends SPL
  case class SETXMLNAMESPACE() extends SPL
  case class ADDCONTEXT() extends SPL
  case class MULTILINE() extends SPL
  case class MULTILINE_BREAK_ON_UNMATCH() extends SPL
  case class SKIP() extends SPL
  case class OBJECT() extends SPL

  trait ICON extends SPL { val icon: Icons }
  case class ICON1(icon: Icons) extends ICON // icon_r
  case class ICON2(icon: Icons, separator: Regex, delimiter: Regex) extends ICON // icon_nvpair_r
  case class ICON3(icon: Icons, key: Regex) extends ICON // icon_nv_unordered_r
  case class ICON4(icon: Icons, xpath: String) extends ICON // icon_xml_r
  case class ICON5(icon: Icons, jpath: String) extends ICON // icon_json_r
  case class ICON6(icon: Icons, horSep: String, verSep: String, multilineKey: String) extends ICON // icon_align_r
  case class ICON7(icon: Icons) extends ICON // multi_table_align_r
  case class ICON8(icon: Icons, logformat: String) extends ICON // icon_apache_r
  case class ICON9(icon: Icons, csvType: String, sep: String) extends ICON // icon_csv_r

}
