package lavender.parse

import java.io.InputStream

import cats.effect.IO
import lavender.parse.Parser._
import lavender.repr.LvFunctionHandle.ByCode

/**
  * Utility methods used by [[Parser]]
  */
object Parser {
  /**
    * Utility for decomposing an arbitrary sequence in head-cons form
    */
  def headTail[A](stream: Stream[A]): (Option[A], Stream[A]) = (stream.headOption, if (stream.isEmpty) Stream.empty else stream.tail)
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

  private def next(): IO[Option[Token]] = tokens.map { ts =>
    val (tok, str) = headTail(ts)
    tokens = IO {
      str
    }
    head = tok.orNull
    tok
  }

  def readDecl(): IO[Option[ByCode]] = next().map { _ =>
    ???
  }
}
