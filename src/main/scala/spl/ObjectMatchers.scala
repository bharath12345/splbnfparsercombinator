package spl
import scala.util.matching.Regex

/**
  * Created by bharadwaj on 31/07/17.
  */
object ObjectMatcher extends RegexMatcher[OBJECT] {
  override protected val regex: Regex = """DEFINE\s+OBJECT\s+(\w+)""".r

  override protected def get(values: String*): OBJECT = OBJECT(values.head)
}

object LabelMatcher extends RegexMatcher[LABEL] {
  override protected val regex: Regex = """LABEL\s+\'(.+?)\'""".r

  override protected def get(values: String*): LABEL = LABEL(values.head)
}

object KeyMatcher extends RegexMatcher[KEY] {
  override protected val regex: Regex = """KEY\s+\((.+?)\)""".r

  override protected def get(values: String*): KEY = {
    val keys = values.head.split(",").view.map(x => x.trim).filter(_.nonEmpty).toSet
    KEY(keys)
  }
}

object ParentMatcher extends RegexMatcher[PARENT] {
  override protected val regex: Regex = """PARENT\s+\((.*)\)""".r

  override protected def get(values: String*): PARENT = {
    PARENT(values.toSet)
  }
}