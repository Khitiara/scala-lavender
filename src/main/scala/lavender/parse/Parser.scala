package lavender.parse

import java.io.InputStream

import cats.effect.IO
import lavender.parse.Parser._

import scala.annotation.tailrec

/**
  * Utility methods used by [[Parser]]
  */
object Parser {
  /**
    * Utility for decomposing an arbitrary sequence in head-cons form
    */
  def headTail[A](stream: Stream[A]): (Option[A], Stream[A]) = (stream.headOption, if (stream.isEmpty) Stream.empty else stream.tail)

  sealed trait ParserState
  case object ReadDecl extends ParserState
  case object ReadImpl extends ParserState
}

/**
  * Parses a lavender source into a set of expressions
  *
  * @param source The source to read from
  */
class Parser(source: InputStream) {
  private val lex = new Lexer(source)
  private var tokens = lex.tokenStream
  private var head: Token = _

  private var curFunName: List[String] = Nil
  private var curArity: List[Int] = Nil
  private var curArgNames: Seq[String] = Seq.empty
  private var curCapNames: Seq[String] = Seq.empty
  private var state: ParserState = ReadDecl


  private def next(): Option[Token] = tokens.map { ts =>
    val (tok, str) = headTail(ts)
    tokens = IO {
      str
    }
    head = tok.orNull
    tok
  }.unsafeRunSync()

  private def require_(typ: TokenType, content: String, pull: Boolean = true): Unit = (if (pull) next() else Option(head)) match {
    case Some(Token(value, tt)) if typ == tt && content == value =>
    case _ => throw LvParserException(s"Required $typ")
  }
  private def require(typ: TokenType, pull: Boolean = true): String = (if (pull) next() else Option(head)) match {
    case Some(Token(value, tt)) if tt == typ => value
    case _ => throw LvParserException(s"Required $typ")
  }

  def read(): Unit = {
    next().foreach(_ => doRead())
  }

  @tailrec
  private final def doRead(): Unit = state match {
    case ReadDecl =>
      Option(head) match {
        case Some(Token("@", TokenType.LITERAL)) =>
          // Command
          val parserCmd = require(TokenType.LITERAL)
        case _ =>
          curCapNames = curArgNames ++ curCapNames
          require_(TokenType.IDENT, "def", pull = false)
          curFunName = require(TokenType.IDENT) :: curFunName
          require_(TokenType.LITERAL, "(")
          while (next().exists(_.tokenType == TokenType.IDENT)) {
            curArgNames
          }
          require_(TokenType.LITERAL, ")", pull = false)
          require_(TokenType.SYMBOL, "=>")
          curArity = curArgNames.length :: curArity

          state = ReadImpl
          next()
          doRead()
      }
    case ReadImpl =>

  }
}
