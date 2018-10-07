package lavender.parse

import Parser._
import java.nio.file.Path

import cats.implicits._
import cats.effect.IO
import lavender.repr.LvFunctionHandle.ByCode

object Parser {
  def headTail[A](stream: Stream[A]): (Option[A], Stream[A]) = (stream.headOption, if (stream.isEmpty) Stream.empty else stream.tail)
}

class Parser(path: Path) {
  private val lex = new Lexer(path)
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
