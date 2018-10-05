package lavender.interpret

import cats.data.Reader
import cats.{Applicative, Eval}
import cats.implicits._
import lavender._
import lavender.expr.{LvCall, LvExpression, LvLiteral, LvParameter}
import lavender.repr.LvFunctionHandle.{ByCode, ByName, ByNative}
import lavender.repr._

class LvInterpreter {

  def unravel(obj: LvObject): Unravel[LvObject] = obj match {
    case s@LvString(_) => Applicative[Unravel].pure(s)
    case LvUndefined => Applicative[Unravel].pure(LvUndefined)
    case i@LvInt(_) => Applicative[Unravel].pure(i)
    case f@LvFloat(_) => Applicative[Unravel].pure(f)
    case v@LvVect(_, _) => Applicative[Unravel].pure(v)
    case LvFunc(ByName(name, _)) => getFunc(name).map(_.map(LvFunc).getOrElse(LvUndefined))
      .flatMap(unravel)
    case f@LvFunc(_) => Applicative[Unravel].pure(f)
  }

  def trace(expr: LvExpression): Unravel[LvObject] = interpret(expr).flatMap(unravel)

  def traceCall[A](fun: LvFunctionHandle, args: LvExpression*)(post: LvObject => A)(env: LvEnvironment): A = {
    if (args.length != fun.arity)
      throw LvInterpreterException("Invalid arity for function called by native code")
    trace(LvCall(fun, args.toIndexedSeq)).map(post).run(env)
  }

  def interpret(expr: LvExpression): Unravel[LvObject] = Reader(env =>
    interpretCall(expr, IndexedSeq.empty, IndexedSeq.empty, env).value)

  private def interpretCall(expr: LvExpression, parameters: IndexedSeq[LvObject], capture: IndexedSeq[LvObject], env: LvEnvironment): Eval[LvObject] = {

    def foldArgs(argV: IndexedSeq[LvExpression]): Eval[IndexedSeq[LvObject]] =
      argV.toList.foldLeftM(IndexedSeq.empty[LvObject]) { case (as, a) => interpretCall(a, parameters, capture, env).map(as :+ _) }

    expr match {
      case LvParameter(i) if i.value < parameters.size => Eval.now(parameters(i.value))
      case LvParameter(i) if i.value >= parameters.size => Eval.now(capture(i.value - parameters.size))
      case LvLiteral(lit) => Eval.now(lit)
      case LvCall(ByCode(code, arity), argV) if argV.size == arity =>
        foldArgs(argV).flatMap(args => interpretCall(code, args, parameters ++ capture, env))
      case LvCall(ByName(name, arity), argV) if argV.size == arity =>
        interpretCall(LvCall(env.lvFuncs(name), argV), parameters, capture, env)
      case LvCall(ByNative(name, arity), argV) if argV.size == arity =>
        foldArgs(argV).map(_.toArray).map(env.nativeFuncs(name)).flatMap(interpretCall(_, parameters, capture, env))

    }
  }
}
