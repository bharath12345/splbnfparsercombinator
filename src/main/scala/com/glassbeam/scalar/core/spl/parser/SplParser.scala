package com.glassbeam.scalar.core.spl.parser

import com.glassbeam.scalar.core.spl.lexer._
import com.glassbeam.scalar.core.spl.parser.TokenSetType.TokenSetType

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.combinator.Parsers

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplPC extends App {
  val tokens: SplTokenList = SplLexer(Source.fromResource("namespace_table.spl"))
  println(s"tokens = $tokens")

  val ast = SplParser(tokens)
  println(s"ast = $ast")
}

object SplParser extends Parsers {

  def apply(tokens: SplTokenList): SplTopLevel = {
    val listOfTokenSets: ListOfSplTokenSets = getListOfSplTokenSets(tokens)
    println(s"listOfTokenSets = $listOfTokenSets")
    val tokenMap: SplTokenMap = validateAndMap(listOfTokenSets)
    println(s"tokenMap = $tokenMap")
    buildAST(tokenMap)
  }

  @tailrec
  private def getListOfSplTokenSets(t: SplTokenList, acc: ListOfSplTokenSets = List()): ListOfSplTokenSets = {
    val (y: SplTokenList, z: SplTokenList) = t.span(_.splToken != EXIT)
    //println(s"y = $y, z = $z")
    if(z.exists(x => x.splToken == EXIT)) getListOfSplTokenSets(z.tail, acc :+ y.toSet)
    else acc.filterNot(_.isEmpty)
  }

  private def validateTokenSet(tokenSet: Set[SplTokenSuperType]): (TokenSetType, String) = {
    def getType(token: SplToken): TokenSetType = {
      token match {
        case _: SplNamespaceToken => TokenSetType.Namespace
        case _: SplTableToken => TokenSetType.Table
        case _: SplObjectToken => TokenSetType.Object
        case _ => throw new Exception(s"Unknown type for token = ${token}")
      }
    }

    def getName(token: SplToken): String = {
      token match {
        case ns: NAMESPACE => ns.name
        case tb: TABLE => tb.name
        case ob: OBJECT => ob.name
        case _ => throw new Exception(s"head token not a Namespace or Table or Object = $token")
      }
    }

    val headType: TokenSetType = getType(tokenSet.head.splToken)
    tokenSet.tail.foreach { token: SplTokenSuperType =>
      val tokenType = getType(token.splToken)
      if(tokenType != headType)
        throw new Exception(s"Inconsistent token type in set = ${tokenSet}")
    }
    (headType, getName(tokenSet.head.splToken))
  }

  private def validateAndMap(listOfTokenSets: ListOfSplTokenSets): SplTokenMap = {
    def loop(listOfTokenSets: ListOfSplTokenSets, acc: SplTokenMap = ListMap()): SplTokenMap = {
      val head = listOfTokenSets.head
      val (tokenType: TokenSetType, tokenName: String) = validateTokenSet(head)

      def inner(newNamedSet: ListMap[String, Set[SplTokenSuperType]]): SplTokenMap = {
        val newacc = acc + (tokenType -> newNamedSet)
        if(listOfTokenSets.tail.nonEmpty) loop(listOfTokenSets.tail, newacc)
        else newacc
      }

      acc.get(tokenType) match {
        case None =>
          inner(ListMap(tokenName -> head))

        case Some(namedTokenSet: ListMap[String, Set[SplTokenSuperType]]) =>
          namedTokenSet.get(tokenName) match {
            case Some(_) =>
              throw new Exception(s"two spl tokens of same type = $tokenType and name = $tokenName found")
            case None =>
              inner(namedTokenSet + (tokenName -> head))
          }
      }
    }

    loop(listOfTokenSets)
  }

  private def buildNamespaceAST(tokenSet: Set[SplTokenSuperType]): NamespaceAST = {
    if(tokenSet.size > 7)
      throw new Exception(s"More than 7 elements in Namespace set: $tokenSet")
    if (tokenSet.size < 2)
      throw new Exception(s"Less than 3 elements in Namespace set: $tokenSet")

    var ast: NamespaceAST = NamespaceAST(null, None, None, None, None, None, None, List(), None, 0)
    tokenSet.foreach {
      case SplTokenSuperType(x: NAMESPACE, _) => ast = ast.copy(namespace = x)
      case SplTokenSuperType(x: BEGINS_WITH, _) => ast = ast.copy(begins = Option(x))
      case SplTokenSuperType(x: ENDS_WITH, _) => ast = ast.copy(ends = Option(x))
      case SplTokenSuperType(x: FILEPATTERN, _) => ast = ast.copy(filepattern = Option(x))
      case SplTokenSuperType(x: CONTEXT, _) => ast = ast.copy(context = Option(x))
      case SplTokenSuperType(x: AS, _) => ast = ast.copy(as = Option(x))
      case SplTokenSuperType(x: BUNDLETYPE, _) => ast = ast.copy(bundletype = Option(x))
      case x => throw new Exception(s"non namespace element found = $x")
    }
    val name = ast.namespace.name
    val ll = name.split("\\.").length - 1 // if level = 1, then it is child of top level namespace
    ast = ast.copy(level = ll)
    println(s"namespace = $name is at level = $ll, ast = $ast")
    ast
  }

  private def addToNamespaceTree(topLevelNamespaceAST: List[NamespaceAST], ast: NamespaceAST): List[NamespaceAST] = {
    println(s"namespace = ${ast.namespace.name} has to put in level = ${ast.level}")

    case class MutableNamespaceAST(namespace: NAMESPACE, begins: Option[BEGINS_WITH], ends: Option[ENDS_WITH],
                                   filepattern: Option[FILEPATTERN], context: Option[CONTEXT], as: Option[AS], bundletype: Option[BUNDLETYPE],
                                   childNamespaces: ListBuffer[MutableNamespaceAST], table: Option[TableAST], level: Int)

    def getMutableNamespaceAST(input: NamespaceAST): MutableNamespaceAST = {
      MutableNamespaceAST(input.namespace, input.begins, input.ends, input.filepattern, input.context, input.as, input.bundletype,
        input.childNamespaces.map(getMutableNamespaceAST).to[ListBuffer], input.table, input.level)
    }

    def getImmutableNamespaceAST(input: MutableNamespaceAST): NamespaceAST = {
      NamespaceAST(input.namespace, input.begins, input.ends, input.filepattern, input.context, input.as, input.bundletype,
        input.childNamespaces.map(getImmutableNamespaceAST).toList, input.table, input.level)
    }

    def findAndAdd(astToTraverse: MutableNamespaceAST): Boolean = {
      if(astToTraverse.level == (ast.level - 1)) {
        val parentName: List[String] = astToTraverse.namespace.name.split("\\.").toList
        val childName: List[String] = ast.namespace.name.split("\\.").toList.init
        if(parentName == childName) {
          val mutableAST = getMutableNamespaceAST(ast)
          astToTraverse.childNamespaces.append(mutableAST)
          true
        } else {
          false
        }
      } else if(astToTraverse.level < (ast.level - 1)) {
        astToTraverse.childNamespaces.find(child => findAndAdd(child)) match {
          case None => false
          case _ => true
        }
      } else
        false
    }

    val mutableList = topLevelNamespaceAST.map(getMutableNamespaceAST)
    if(!mutableList.exists(child => findAndAdd(child))) {
      throw new Exception(s"could not add child namespace to the tree: $ast")
    }

    mutableList.map(getImmutableNamespaceAST)
  }

  private def buildNamespaceAST(namespaces: ListMap[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    var topLevelNamespaceAST: List[NamespaceAST] = List()
    namespaces.foreach { case (name, tokenSet) =>
      val ast = buildNamespaceAST(tokenSet)
      topLevelNamespaceAST =
        if(ast.level > 0) addToNamespaceTree(topLevelNamespaceAST, ast)
        else ast +: topLevelNamespaceAST
    }
    topLevelNamespaceAST
  }

  private def buildTableAST(tokens: Set[SplTokenSuperType]): TableAST = {
    var ast: TableAST = TableAST(null, null, null, List(), List(), None, None, None, None, None, None)
    tokens.foreach {
      case SplTokenSuperType(x: TABLE, _) => ast = ast.copy(table = x)
      case SplTokenSuperType(x: ICON, _) => ast = ast.copy(icon = x)
      case SplTokenSuperType(x: COLUMN, _) => val cols = x +: ast.columns; ast.copy(columns = cols)
      case SplTokenSuperType(x: LINEGRAB, _) => ast = ast.copy(linegrab = Option(x))
      case SplTokenSuperType(x: SETXMLNAMESPACE, _) => ast = ast.copy(setXmlNs = Option(x))
      case SplTokenSuperType(x: ADDCONTEXT, _) => ast = ast.copy(addContext = Option(x))
      case SplTokenSuperType(x: MULTILINE, _) => ast = ast.copy(multiline = Option(x))
      case SplTokenSuperType(x: MULTILINE_BREAK_ON_UNMATCH, _) => ast = ast.copy(multilineBOU = Option(x))
      case SplTokenSuperType(x: SKIP, _) => ast = ast.copy(skip = Option(x))
      case x => throw new Exception(s"non table element found = $x")
    }
    ast
  }

  private def addTableToTree(topLevelNamespaceAST: List[NamespaceAST], table: TableAST): List[NamespaceAST] = {
    val tblNamespaceName: String = table.table.namespace

    case class MutableNamespaceAST(namespace: NAMESPACE, begins: Option[BEGINS_WITH], ends: Option[ENDS_WITH],
                                   filepattern: Option[FILEPATTERN], context: Option[CONTEXT], as: Option[AS], bundletype: Option[BUNDLETYPE],
                                   childNamespaces: List[MutableNamespaceAST], var table: Option[TableAST], level: Int)


    def getMutableNamespaceAST(input: NamespaceAST): MutableNamespaceAST = {
      MutableNamespaceAST(input.namespace, input.begins, input.ends, input.filepattern, input.context, input.as, input.bundletype,
        input.childNamespaces.map(getMutableNamespaceAST), input.table, input.level)
    }

    def getImmutableNamespaceAST(input: MutableNamespaceAST): NamespaceAST = {
      NamespaceAST(input.namespace, input.begins, input.ends, input.filepattern, input.context, input.as, input.bundletype,
        input.childNamespaces.map(getImmutableNamespaceAST), input.table, input.level)
    }

    def findAndAdd(astToTraverse: MutableNamespaceAST): Boolean = {
      if(astToTraverse.namespace.name == tblNamespaceName) {
        val newTable = table.copy(namespace = getImmutableNamespaceAST(astToTraverse))
        astToTraverse.table = Option(newTable)
        true
      } else {
        astToTraverse.childNamespaces.find(child => findAndAdd(child)) match {
          case None => false
          case _ => true
        }
      }
    }

    val mutableList = topLevelNamespaceAST.map(getMutableNamespaceAST)
    if(!mutableList.exists(child => findAndAdd(child))) {
      throw new Exception(s"could not add table to the tree: $table")
    }

    mutableList.map(getImmutableNamespaceAST)
  }

  private def buildTableAST(namespaces: List[NamespaceAST], tables: ListMap[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    val tableASTs: List[TableAST] = tables.values.map(buildTableAST).toList
    var namespacesWithTbl = namespaces
    tableASTs.foreach { table =>
      namespacesWithTbl = addTableToTree(namespacesWithTbl, table)
    }
    namespacesWithTbl
  }

  private def buildObjectAST(objects: ListMap[String, Set[SplTokenSuperType]]): List[ObjectAST] = {
    objects.map { case (_, superTokens: Set[SplTokenSuperType]) =>
      if(superTokens.size != 4)
        throw new Exception(s"num of object elements not equal to 4: $superTokens")
      var ast: ObjectAST = null
      superTokens.foreach {
        case SplTokenSuperType(x: OBJECT, _) => ast = ast.copy(obj = x)
        case SplTokenSuperType(x: LABEL, _) => ast = ast.copy(label = x)
        case SplTokenSuperType(x: KEY, _) => ast = ast.copy(key = x)
        case SplTokenSuperType(x: PARENT, _) => ast = ast.copy(parent = x)
        case x => throw new Exception(s"non object element found = $x")
      }
      ast
    }.toList
  }

  private def buildAST(tokenMap: SplTokenMap): SplTopLevel = {
    /*
    1. traverse the list of namespaces and build the hierarchy of NamespaceAST. finally there has to be a list of top level NamespaceAST
    2. traverse the list of tables and plug each one into corresponding NamespaceAST
    3. build SplTopLevel: this has list of top level NamespaceAST and list of all objects
     */

    val namespaceAST = tokenMap.get(TokenSetType.Namespace) match {
      case None => throw new Exception(s"no namespaces in the spl!")
      case Some(namespaces: ListMap[String, Set[SplTokenSuperType]]) => buildNamespaceAST(namespaces)
    }
    println(s"completed namespaces AST = $namespaceAST")

    val namespaceTableAST = tokenMap.get(TokenSetType.Table) match {
      case None => throw new Exception(s"no tables in this spl!")
      case Some(tables: ListMap[String, Set[SplTokenSuperType]]) => buildTableAST(namespaceAST, tables)
    }
    println(s"completed namespace and table AST = $namespaceTableAST")

    val objectAST: List[ObjectAST] = buildObjectAST(tokenMap.getOrElse(TokenSetType.Object, ListMap()))
    val fullAST = SplTopLevel(namespaceTableAST, objectAST)
    println(s"completed full AST = $fullAST")
    fullAST
  }
}