package spl

import spl.lexer._

import scala.util.parsing.input.Positional

/**
  * Created by bharadwaj on 31/07/17.
  */
package object parser {

  sealed trait SplAST extends Positional
  // for starters, using basic building block as a list of namespaces, tables and object definitions
  // ideally, want two rules in the AST :
  // 1. table can be contained only in a namespace
  // 2. one namespace can contain another; and this can be infinitely deep
  case class Blocks(list: List[SplAST]) extends SplAST
  case class NamespaceAST(namespace: NAMESPACE, begins: Option[BEGINS_WITH], ends: Option[ENDS_WITH], filepattern: Option[FILEPATTERN]) extends SplAST
  case class TableAST(table: TABLE, icon: ICON, columns: List[COLUMN], linegrab: Option[LINEGRAB], setXmlNs: Option[SETXMLNAMESPACE],
                      addContext: Option[ADDCONTEXT], multiline: Option[MULTILINE], multilineBOU: Option[MULTILINE_BREAK_ON_UNMATCH],
                      skip: SKIP) extends SplAST
  case class ObjectAST(obj: OBJECT, label: LABEL, key: KEY, parent: PARENT) extends SplAST
}
