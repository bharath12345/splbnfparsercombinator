package spl

import spl.lexer._

/**
  * Created by bharadwaj on 31/07/17.
  */
package object parser {

  // two rules in the AST :
  // 1. table can be contained only in a namespace
  // 2. one namespace can contain another; and this can be infinitely deep

  sealed trait SplAST

  case class SplTopLevel(namespaces: List[NamespaceAST], objects: List[ObjectAST]) extends SplAST

  case class NamespaceAST(namespace: NAMESPACE, begins: Option[BEGINS_WITH], ends: Option[ENDS_WITH],
                          filepattern: Option[FILEPATTERN], childNamespace: Option[NamespaceAST], table: Option[TableAST]) extends SplAST

  case class TableAST(namespace: NamespaceAST, table: TABLE, icon: ICON, columns: List[COLUMN], linegrab: Option[LINEGRAB],
                      setXmlNs: Option[SETXMLNAMESPACE], addContext: Option[ADDCONTEXT], multiline: Option[MULTILINE],
                      multilineBOU: Option[MULTILINE_BREAK_ON_UNMATCH], skip: SKIP) extends SplAST

  case class ObjectAST(obj: OBJECT, label: LABEL, key: KEY, parent: PARENT) extends SplAST
}