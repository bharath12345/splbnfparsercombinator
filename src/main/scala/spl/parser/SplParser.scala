package spl.parser

import spl.lexer.{AS, BEGINS_WITH, BUNDLETYPE, CONTEXT, ENDS_WITH, EXIT, FILEPATTERN, KEY, LABEL, NAMESPACE, OBJECT, PARENT, SplLexer, SplNamespaceToken, SplObjectToken, SplTableToken, SplToken, SplTokenSuperType, TABLE}
import spl.parser.TokenSetType.TokenSetType

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.parsing.combinator.Parsers

/**
  * Created by bharadwaj on 31/07/17.
  */
object SplPC extends App {
  val tokens: SplTokenList = (for {
    (code, linenum) <- Source.fromResource("namespace_table.spl").getLines().zipWithIndex
    line = code.trim
    if line.nonEmpty && line.head != '#'
  } yield {
    SplLexer(line, linenum).right.get
  }).toList

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
    if(z.exists(x => x.splToken == EXIT)) getListOfSplTokenSets(z.tail, y.toSet :: acc)
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

  private def namespaceAST(tokenSet: Set[SplTokenSuperType]): NamespaceAST = {
    if(tokenSet.size > 7)
      throw new Exception(s"More than 7 elements in Namespace set: $tokenSet")
    if (tokenSet.size < 3)
      throw new Exception(s"Less than 3 elements in Namespace set: $tokenSet")

    var ast: NamespaceAST = NamespaceAST(null, None, None, None, None, None, None, List(), None)
    tokenSet.foreach {
      case SplTokenSuperType(x: NAMESPACE, _) => ast = ast.copy(namespace = x)
      case SplTokenSuperType(x: BEGINS_WITH, _) => ast = ast.copy(begins = Option(x))
      case SplTokenSuperType(x: ENDS_WITH, _) => ast = ast.copy(ends = Option(x))
      case SplTokenSuperType(x: FILEPATTERN, _) => ast = ast.copy(filepattern = Option(x))
      case SplTokenSuperType(x: CONTEXT, _) => ast = ast.copy(context = Option(x))
      case SplTokenSuperType(x: AS, _) => ast = ast.copy(as = Option(x))
      case SplTokenSuperType(x: BUNDLETYPE, _) => ast = ast.copy(bundletype = Option(x))
      case x => throw new Exception(s"non object element found = $x")
    }
    ast
  }

  private def addToNamespaceTree(topLevelNamespaceAST: List[NamespaceAST], ast: NamespaceAST): List[NamespaceAST] = {
    null
  }

  private def buildNamespaceAST(namespaces: ListMap[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    var topLevelNamespaceAST: List[NamespaceAST] = List()
    namespaces.foreach { case (name, tokenSet) =>
      val ast = namespaceAST(tokenSet)
      val topLevel: Boolean = name.contains(".")
      topLevelNamespaceAST = if(topLevel) {
        addToNamespaceTree(topLevelNamespaceAST, ast)
      } else {
        ast +: topLevelNamespaceAST
      }
    }
    topLevelNamespaceAST
  }

  private def buildTableAST(namespaces: List[NamespaceAST], tables: ListMap[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    null
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

    val namespaceTableAST = tokenMap.get(TokenSetType.Table) match {
      case None => throw new Exception(s"no tables in this spl!")
      case Some(tables: ListMap[String, Set[SplTokenSuperType]]) => buildTableAST(namespaceAST, tables)
    }

    val objectAST = buildObjectAST(tokenMap.getOrElse(TokenSetType.Object, ListMap()))

    SplTopLevel(namespaceTableAST, objectAST)
  }
}