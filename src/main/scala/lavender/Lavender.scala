package lavender

import java.nio.file.{Files, Path}

import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import lavender.parse.Grammar.{CommentOrBlank, InterpCommand, ParsedExpression}
import lavender.parse.{GrammarParser, LvParserException}
import lavender.util.monocles._

object Lavender {
  def init(): Unit = {

  }

  def processReplCommand(cmd: String): Repl[Unit] = cmd.split("\\s+", 2).toList match {
    case "import" :: name :: rest =>
      processImport(name) // TODO: Handle using clause, for now not implemented
    case _ => Repl.throws(LvParserException(s"Unrecognized interpreter command: ${cmd.trim()}"))
  }

  // These two may mutually recurse but who cares, IO is stack-safe anyway
  def readFile(path: Path): StateT[IO, LvEnvironment, Unit] = for {
    parser <- Repl.pure(new GrammarParser(Files.newInputStream(path)))
    lines <- parser.run[Repl]
    _ <- lines.traverse_[Repl, Unit] {
      case InterpCommand(cmd) => processReplCommand(cmd)
      case ParsedExpression(expr) => ??? //TODO: Things
      case CommentOrBlank => Repl.void
    }
  } yield Unit
  def processImport(name: String): StateT[IO, LvEnvironment, Unit] = for {
    srcPath <- LvEnvironment.lp.extract[IO]
    path = srcPath.resolve(name)
    _ <- path.traverse(readFile)
  } yield Unit
}
