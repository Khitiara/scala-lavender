package lavender.test

import java.io.ByteArrayInputStream

import lavender.parse.Lexer

import scala.io.Codec

object LexerTesting {
  def main(args: Array[String]): Unit = {
    val src =
      """
        | def blah(x) => x + "hi there \n\" peoples of the world!"
        |
        | @import blah
        | def glargle(x, y) => def() => x + y
      """.stripMargin
    val source = new ByteArrayInputStream(Codec.toUTF8(src))
    val lexer = new Lexer(source)
    println("reading simple thing")
    while (true) {
      val tok = lexer.nextTok().unsafeRunSync()
      println(tok)
      if (tok.isEmpty) {
        lexer.close()
        return
      }
    }
  }
}
