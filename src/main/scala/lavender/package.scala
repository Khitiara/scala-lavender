import cats.data.Reader
import lavender.expr.LvExpression
import lavender.repr.{LvFunctionHandle, LvObject}
import lavender.util.SnowflakeType

package object lavender {

  trait FunctionTag

  type FunctionName = SnowflakeType[FunctionTag]

  object FunctionName {
    def apply(s: String): FunctionName = SnowflakeType[FunctionTag](s)
  }

  type LvNativeFunc = Array[LvObject] => LvExpression

  type Unravel[A] = Reader[LvEnvironment, A]

  def getFunc(name: FunctionName): Unravel[Option[LvFunctionHandle]] = Reader(_.lvFuncs.get(name))

  def getNative(name: FunctionName): Unravel[Option[LvNativeFunc]] = Reader(_.nativeFuncs.get(name))
}
