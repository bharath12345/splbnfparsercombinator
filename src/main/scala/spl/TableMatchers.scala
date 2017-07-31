package spl

import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object TableMatcher extends RegexMatcher[TABLE] {
  override protected def get(values: String*) = {
    val value_list = values.toList
    val name = value_list.head
    val ns = value_list(1)
    val desc = Option(value_list(2))
    TABLE(name, ns, desc)
  }

  override protected val regex: Regex = """^DEFINE\s+TABLE\s+([\w_\{\}]+)\s+NAMESPACE\s+([\w\.\{\}]+)\s*(DESCRIPTION\s+'.+?'|)\s*""".r
}
