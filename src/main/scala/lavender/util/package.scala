package lavender

import shapeless.tag.{@@, Tagger}

package object util {

  private object SharedTagger extends Tagger[Nothing]
  private def tagS[A]: Tagger[SnowflakeTag[A]] = SharedTagger.asInstanceOf[Tagger[SnowflakeTag[A]]]

  trait SnowflakeTag[A]

  type SnowflakeType[A] = String @@ SnowflakeTag[A]
  object SnowflakeType {
    def apply[A](l: String): SnowflakeType[A] = tagS[A].apply[String](l)
  }
}
