package spl.parser

import spl.lexer.{EXIT, KEY, LABEL, NAMESPACE, OBJECT, PARENT, SplLexer, SplNamespaceToken, SplObjectToken, SplTableToken, SplToken, SplTokenSuperType, TABLE}
import spl.parser.TokenSetType.TokenSetType

import scala.annotation.tailrec
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
    def loop(listOfTokenSets: ListOfSplTokenSets, acc: SplTokenMap = Map()): SplTokenMap = {
      val head = listOfTokenSets.head
      val (tokenType: TokenSetType, tokenName: String) = validateTokenSet(head)

      def inner(newNamedSet: Map[String, Set[SplTokenSuperType]]): SplTokenMap = {
        val newacc = acc + (tokenType -> newNamedSet)
        if(listOfTokenSets.tail.nonEmpty) loop(listOfTokenSets.tail, newacc)
        else newacc
      }

      acc.get(tokenType) match {
        case None =>
          inner(Map(tokenName -> head))

        case Some(namedTokenSet) =>
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

  private def buildNamespaceAST(namespaces: Map[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    null
  }

  private def buildTableAST(namespaces: List[NamespaceAST], tables: Map[String, Set[SplTokenSuperType]]): List[NamespaceAST] = {
    null
  }

  private def buildObjectAST(objects: Map[String, Set[SplTokenSuperType]]): List[ObjectAST] = {
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
      case Some(namespaces) => buildNamespaceAST(namespaces)
    }

    val namespaceTableAST = tokenMap.get(TokenSetType.Table) match {
      case None => throw new Exception(s"no tables in this spl!")
      case Some(tables) => buildTableAST(namespaceAST, tables)
    }

    val objectAST = buildObjectAST(tokenMap.getOrElse(TokenSetType.Object, Map()))

    SplTopLevel(namespaceTableAST, objectAST)
  }
}