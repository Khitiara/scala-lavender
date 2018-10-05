import cats.data.Reader
import lavender.expr.{LvExpression, LvLiteral}
import lavender.repr.{LvFunctionHandle, LvObject}
import lavender.util.SnowflakeType

import scala.language.implicitConversions

package object lavender {

  trait FunctionTag

  type FunctionName = SnowflakeType[FunctionTag]

  object FunctionName {
    def apply(s: String): FunctionName = SnowflakeType[FunctionTag](s)
  }

  type LvNativeFunc = Array[LvObject] => LvExpression
  type LvNativeFunc1 = Array[LvObject] => LvObject

  implicit def wrap(f: LvNativeFunc1): LvNativeFunc = f.andThen(LvLiteral)

  type Unravel[A] = Reader[LvEnvironment, A]

  def getFunc(name: FunctionName): Unravel[Option[LvFunctionHandle]] = Reader(_.lvFuncs.get(name))

  def getNative(name: FunctionName): Unravel[Option[LvNativeFunc]] = Reader(_.nativeFuncs.get(name))
}
