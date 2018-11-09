import cats.data.StateT
import cats.effect.IO
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

  type Repl[A] = StateT[IO, LvEnvironment, A]

  object Repl {
    def get: Repl[LvEnvironment] = StateT.get
    def read[A](f: LvEnvironment => A): Repl[A] = get.map(f)
    def pure[A](f: => A): Repl[A] = StateT.pure(f)
    def run[A](f: Repl[A], env: LvEnvironment): A = f.runA(env).unsafeRunSync()
    def void: Repl[Unit] = pure(Unit)
    def throws[A](throwable: Throwable): Repl[A] = pure(throw throwable)
  }

  def getFunc(name: FunctionName): Repl[Option[LvFunctionHandle]] = Repl.read(_.lvFuncs.get(name))

  def getNative(name: FunctionName): Repl[Option[LvNativeFunc]] = Repl.read(_.nativeFuncs.get(name))
}
