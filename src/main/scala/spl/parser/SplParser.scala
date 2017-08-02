package spl.parser

import spl.lexer.{EXIT, NAMESPACE, OBJECT, SplLexer, SplNamespaceToken, SplObjectToken, SplTableToken, SplToken, TABLE}
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
    if line.nonEmpty
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
    val (y: SplTokenList, z: SplTokenList) = t.span(_ != EXIT)
    //println(s"y = $y, z = $z")
    if(z.contains(EXIT)) getListOfSplTokenSets(z.tail, y.toSet :: acc)
    else acc.filterNot(_.isEmpty)
  }

  private def validateTokenSet(tokenSet: Set[SplToken]): (TokenSetType, String) = {
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

    val headType: TokenSetType = getType(tokenSet.head)
    tokenSet.tail.foreach { token: SplToken =>
      val tokenType = getType(token)
      if(tokenType != headType)
        throw new Exception(s"Inconsistent token type in set = ${tokenSet}")
    }
    (headType, getName(tokenSet.head))
  }

  private def validateAndMap(listOfTokenSets: ListOfSplTokenSets): SplTokenMap = {

    def loop(listOfTokenSets: ListOfSplTokenSets, acc: SplTokenMap = Map()): SplTokenMap = {
      val head = listOfTokenSets.head
      val (tokenType: TokenSetType, tokenName: String) = validateTokenSet(head)

      def inner(newNamedSet: Map[String, Set[SplToken]]) = {
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

  private def buildAST(tokenMap: SplTokenMap): SplTopLevel = {
    null
  }

}