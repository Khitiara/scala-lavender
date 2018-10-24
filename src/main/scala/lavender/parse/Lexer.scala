package lavender.parse

import java.io.{Closeable, InputStream, InputStreamReader}

import cats.Eval
import cats.effect.IO
import cats.implicits._
import lavender.parse.Lexer._
import lavender.parse.TokenType._
import scala.io.Codec

/**
  * Companion object to [[Lexer]], holds basic utility methods for checking for certain kinds of input
  */
object Lexer {
  def isidbgn(ch: Char): Boolean = ch.isLetter || ch == '_'

  def issymb(ch: Char): Boolean = "~!%^&*-+=|<>/?:".contains(ch)

  def isident(ch: Char): Boolean = isidbgn(ch) || ch.isDigit
}

/**
  * A lexer which tokenizes an input according to the lavender spec
  *
  * @param source The input source to read from
  */
//noinspection AccessorLikeMethodIsUnit
//We use the same naming convention as clavender, so suppress the warnings about naming things
class Lexer(source: InputStream) extends Closeable {
  type Lex = Option[Token]

  private def err(t: String): Lex = throw LvLexerException(t)

  private def isEoi = idx >= contents.length

  private def pullBuf(max: Int): Array[Char] = {
    val arr: Array[Char] = new Array(max)
    val n = charReader.read(arr)
    if (n <= 0) return Array('\u0000')
    val cs = new Array[Char](n)
    Array.copy(arr, 0, cs, 0, n)
    cs
  }

  private lazy val charReader: InputStreamReader = new InputStreamReader(source, Codec.UTF8.charSet)
  private lazy val contents: Array[Char] = Stream.continually(charReader.read()).takeWhile(_ != -1).map(_.toChar).toArray
  private var idx = -1
  private var checkedShebang: Boolean = false

  private var parenNesting = 0
  private var bracketNesting = 0
  private var braceNesting = 0

  private def ch: Char = contents(idx)

  private val buf: StringBuilder = StringBuilder.newBuilder
  private var tokenType: TokenType = _

  private def next(): Char = {
    buf.append(ch)
    idx += 1
    ch
  }

  private def discard(): Unit = buf.clear()

  private def rewind(): Unit = {
    idx -= buf.size
    discard()
  }

  private def pull(): String = {
    val s = buf.mkString
    discard()
    s
  }

  /**
    * Creates a continual stream of [[Token]], which will terminate when EOF is reached
    */
  def tokenStream: IO[Stream[Token]] = Stream.continually(nextTok()).sequence[IO, Option[Token]]
    .map(_.takeWhile(_.nonEmpty).map(_.get))

  def nextTok(): IO[Option[Token]] = IO(read.value)

  private def pullWhile(f: Char => Boolean): Unit = {
    next()
    while (f(ch)) {
      next()
    }
  }

  private def discardLine(): Unit = {
    while (ch != '\n') next()
    discard()
    if (ch == '\r') next() // Fuck windows
  }

  private def tryGetQualName(): Unit = {
    tokenType = IDENT
    pullWhile(isident)
    if (ch == ':') {
      next()
      if (isidbgn(ch)) {
        pullWhile(isident)
        tokenType = QUAL_IDENT
      } else if (issymb(ch)) {
        pullWhile(issymb)
        tokenType = QUAL_SYMBOL
      } else {
        err("BAD_QUAL")
      }
    }
  }

  private def tryGetFuncSym(): Unit = {
    if (ch == 'u' || ch == 'i' || ch == 'r') {
      if (next() == '_' && issymb(next())) {
        pullWhile(issymb)
        tokenType = FUNC_SYMBOL
      }
      else {
        rewind()
        tryGetQualName()
      }
    } else {
      rewind()
      tryGetQualName()
    }
    idx -= 1
  }

  private def getSymbol(): Unit = {
    pullWhile(issymb)
    tokenType = SYMBOL
  }

  private def getNumber(): Unit = {
    if (ch.isDigit) {
      pullWhile(_.isDigit)
      if (ch != '.') {
        tokenType = INTEGER
        return
      }
    }
    assert(ch == '.')
    next()
    if (!ch.isDigit)
      err("BAD_NUM")
    pullWhile(_.isDigit)
    if (ch == 'e' || ch == 'E') {
      next()
      if (ch == '+' || ch == '-') {
        next()
      }
      if (!ch.isDigit)
        err("BAD_EXP")
      pullWhile(_.isDigit)
    }
    tokenType = FLOATING
  }

  private def tryGetEllipses(): Unit = {
    pullWhile(_ == '.')
    if (buf.length == 3) {
      tokenType = ELLIPSES
      return
    }
    rewind()
    getNumber()
  }

  private def getFuncVal(): Unit = {
    next()
    if (isEoi)
      err("BAD_FUNC_VAL")
    if (issymb(ch)) {
      pullWhile(issymb)
      tokenType = FUNC_VAL
    } else if (isidbgn(ch)) {
      tryGetQualName()
      if (tokenType == QUAL_IDENT || tokenType == QUAL_SYMBOL) {
        tokenType = QUAL_FUNC_VAL
      } else tokenType = FUNC_VAL
    } else {
      err("BAD_FUNC_VAL")
    }
    if (ch == '\\') {
      next()
      discard()
    }
  }

  private def getString(): Unit = {
    assert(ch == '"')
    do {
      if (ch == '\\') {
        idx += 1
        ch match {
          case 'n' =>
            idx += 1
            buf.append('\n')
          case 't' =>
            idx += 1
            buf.append('\t')
          case '\'' | '"' | '\\' =>
            next()
            if (isEoi)
              err("UNTERM_STR")
          case _ =>
            err("BAD_STR_ESC")
        }
      } else if (ch == '\n') {
        err("BAD_STR_CHR")
        next()
        if (isEoi)
          err("UNTERM_STR")
      } else {
        next()
      }
    } while (ch != '"')
    idx += 1 // Consume closing quote
    tokenType = STRING
    buf.deleteCharAt(0)
  }

  private def getLiteral(): Unit = {
    ch match {
      case '(' => parenNesting += 1
      case ')' => parenNesting -= 1
      case '[' => bracketNesting += 1
      case ']' => bracketNesting -= 1
      case '{' => braceNesting += 1
      case '}' => braceNesting -= 1
      case _ =>
    }
    if (parenNesting < 0 || braceNesting < 0 || bracketNesting < 0)
      err("UNBAL_PAREN")
    tokenType = LITERAL
  }

  private def read: Eval[Lex] = {
    idx += 1
    if (isEoi)
      return Eval.now(None)
    if (!checkedShebang) {
      if (ch == '#') {
        next()
        if (ch != '!') {
          err("Invalid shebang")
        }
        discardLine()
      }
      checkedShebang = true
    }
    if ("(){}[]".contains(ch)) {
      buf.append(ch)
      getLiteral()
    } else if (ch == '\'') {
      discardLine()
      return Eval.defer(read)
    } else if (ch == ' ' || ch.isWhitespace) {
      return Eval.defer(read)
    } else if (isidbgn(ch)) {
      tryGetFuncSym()
    } else if (issymb(ch)) {
      getSymbol()
    } else if (ch.isDigit) {
      getNumber()
    } else if (ch == '.') {
      tryGetEllipses()
    } else if (ch == '\\') {
      getFuncVal()
    } else if (ch == '"') {
      getString()
    } else {
      buf.append(ch)
      getLiteral()
    }


    if (tokenType == null)
      return Eval.defer(read)

    val str = pull()
    if (str.isEmpty) {
      println(s"UHHWAT $tokenType")
      return Eval.defer(read)
    }
    val someToken = Eval.now(Some(Token(str, tokenType)))
    tokenType = null
    someToken
  }
  override def close(): Unit = charReader.close()
}
