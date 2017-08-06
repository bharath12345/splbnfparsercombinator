package com.glassbeam.scalar.core.spl

package object parser {

  type ListOfSplTokenSets = List[Set[SplTokenSuperType]]
  type SplTokenList = List[SplTokenSuperType]
  type SplTokenMap = Map[TokenSetType, ListMap[String, Set[SplTokenSuperType]]]

  // two rules in the AST :
  // 1. table can be contained only in a namespace
  // 2. one namespace can contain another; and this can be infinitely deep

  sealed trait SplAST

  case class SplTopLevel(namespaces: List[NamespaceAST], objects: List[ObjectAST]) extends SplAST {
    override def toString: String = {
      namespaces.mkString("\n") + "\n" + objects.mkString("\n")
    }
  }

  case class NamespaceAST(namespace: NAMESPACE, begins: Option[BEGINS_WITH], ends: Option[ENDS_WITH],
                          filepattern: Option[FILEPATTERN], context: Option[CONTEXT], as: Option[AS], bundletype: Option[BUNDLETYPE],
                          childNamespaces: List[NamespaceAST], table: Option[TableAST], level: Int) extends SplAST {

    //private def tabs(ll: Int): String = {var s = ""; for(i <- 1 to ll) s = s + "\n\t"; s}

    private def children: String = if(childNamespaces.isEmpty) "" else s", childNamespaces: ${childNamespaces.map(_.toString)}"

    override def toString: String = {
      def emptyIfNone(o: Option[SplNamespaceToken], title: String): String = if(o.isEmpty) "" else s"$title = ${o.get.toString},"

      s"NamespaceAST($namespace, ${emptyIfNone(begins, "begins with")} ${emptyIfNone(ends, "ends with")} " +
        s"${emptyIfNone(filepattern, "filepattern")} ${emptyIfNone(context, "context")} ${emptyIfNone(as, "as")} " +
        s"${emptyIfNone(bundletype, "bundletype")} table = $table, level = $level $children)"
    }
  }

  case class TableAST(namespace: NamespaceAST, table: TABLE, icon: ICON, columns: List[COLUMN], colops: List[ColOpTrait], linegrab: Option[LINEGRAB],
                      setXmlNs: Option[SETXMLNAMESPACE], addContext: Option[ADDCONTEXT], multiline: Option[MULTILINE],
                      multilineBOU: Option[MULTILINE_BREAK_ON_UNMATCH], skip: Option[SKIP]) extends SplAST

  case class ObjectAST(obj: OBJECT, label: LABEL, key: KEY, parent: PARENT) extends SplAST

  object TokenSetType extends Enumeration {
    type TokenSetType = Value
    val Namespace, Table, Object = Value
  }
}
