package lavender

import java.io.InputStream

import atto.fs2.Pipes
import cats.effect.{ContextShift, IO}
import fs2.text.utf8Decode
import shapeless.tag.{@@, Tagger}

import scala.concurrent.ExecutionContext

package object util {

  private object SharedTagger extends Tagger[Nothing]
  private def tagS[A]: Tagger[SnowflakeTag[A]] = SharedTagger.asInstanceOf[Tagger[SnowflakeTag[A]]]

  trait SnowflakeTag[A]

  type SnowflakeType[A] = String @@ SnowflakeTag[A]
  object SnowflakeType {
    def apply[A](l: String): SnowflakeType[A] = tagS[A].apply[String](l)
  }

  def feedStream[A](p: atto.Parser[A], is: InputStream, bufSize: Int = 256): IO[List[A]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    val pipe = fs2.io.readInputStream(IO.pure(is), bufSize, ExecutionContext.global)
    pipe.through(utf8Decode).through(Pipes.parseN(p)).compile.toList
  }
}
