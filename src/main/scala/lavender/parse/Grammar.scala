package lavender.parse

import java.io.InputStream

import atto.Atto._
import cats.effect.IO
import cats.implicits._
import lavender.expr._
import lavender.repr._
import lavender.util.feedStream

import scala.language.implicitConversions

object Grammar {
  // -------------------------------------------------------------------------------------------------------------------
  // Helpers
  implicit class RichParser0(p: atto.Parser[Char]) {
    def ~~(p1: atto.Parser[Char]): atto.Parser[String] = (p ~ p1) -| { case (a, b) => new String(Array(a, b)) }
    def ~~~(p1: atto.Parser[String]): atto.Parser[String] = (p ~ p1) -| { case (a, b) => a +: b }
  }
  implicit class RichParser1(p: atto.Parser[String]) {
    def ~~(p1: atto.Parser[Char]): atto.Parser[String] = (p ~ p1) -| { case (a, b) => a :+ b }
    def ~~~(p1: atto.Parser[String]): atto.Parser[String] = (p ~ p1) -| { case (a, b) => a ++ b }
  }

  sealed abstract class ArgsList(val arity: Int)
  case class Varargs(name: String) extends ArgsList(-1)
  case class Cdecl(names: Map[Int, String]) extends ArgsList(names.size) // Bad name but I dont care
  case class FunSig(name: Option[String], args: ArgsList)

  sealed trait LvParseBlock
  case class ParsedExpression(expr: LvExpression) extends LvParseBlock
  case class InterpCommand(line: String) extends LvParseBlock // to handle elsewhere
  case object CommentOrBlank extends LvParseBlock

  def tok(tt: TokenType)(str: String) = Token(str, tt)

  def newline: atto.Parser[String] = takeWhile1(c => c === '\r' || c === '\n')
  def consumeLine: atto.Parser[String] = takeWhile(c => c =!= '\n' && c =!= '\r') <~ newline
  def whiteSpaceLine: atto.Parser[String] = stringOf(horizontalWhitespace) <~ newline

  // -------------------------------------------------------------------------------------------------------------------
  // Lavender grammar
  // This is where the work gets done. Final output had gosh dang better allow for an LvExpression somehow
  val number: atto.Parser[LvLiteral] = (bigInt.map[LvObject](LvInt) | double.map[LvObject](LvFloat)) -| LvLiteral

  val stringLit: atto.Parser[LvLiteral] = {
    val nesc: atto.Parser[Char] =
      elem(c => c =!= '\\' && c =!= '"' && !c.isControl)
    val escapes = char('\\') ~> choice(
      char('n') >| '\n',
      char('t') >| '\t',
      oneOf("\'\"\\")
    )
    bracket(char('"'), stringOf(nesc | escapes), char('"')) named "lv-string"
  } -| LvString -| LvLiteral

  val ellipses: atto.Parser[String] = string("...")

  val funcSymb: atto.Parser[String] = oneOf("uir") ~~ char('_') ~~~ takeWhile(Lexer.issymb)

  val qualifiedName: atto.Parser[String] = for {
    ns <- takeWhile(Lexer.isident)
    colon <- char(':')
    name <- takeWhile(Lexer.isident) | takeWhile(Lexer.issymb)
  } yield s"$ns$colon$name"

  val ident: atto.Parser[String] = takeWhile(Lexer.isident)

  val argsList: atto.Parser[ArgsList] =
    parens(sepBy(opt(token(string("=>"))) ~> ident, token(char(','))).map(_.zipWithIndex.map(_.swap).toMap).map(Cdecl) | (ellipses ~> ident -| Varargs))

  val funSig: atto.Parser[FunSig] = for {
    _ <- string("def")
    _ <- skipWhitespace
    name <- token(opt(funcSymb | ident))
    args <- token(argsList)
  } yield FunSig(name, args)

  val interpCommand: atto.Parser[InterpCommand] = char('@') ~> consumeLine -| InterpCommand

  val expr: atto.Parser[LvExpression] = ???

  val comment: atto.Parser[Unit] = char('\'') ~> consumeLine.void
  val pull: atto.Parser[LvParseBlock] = (comment | whiteSpaceLine.void) >| CommentOrBlank

  val blockParser: atto.Parser[LvParseBlock] = interpCommand | expr.map[LvParseBlock](ParsedExpression) | pull
}

class GrammarParser(source: InputStream) {
  def run: IO[List[Grammar.LvParseBlock]] = feedStream(Grammar.blockParser, source)
}
