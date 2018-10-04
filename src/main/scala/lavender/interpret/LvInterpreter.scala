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
    case LvVect(vec, size) => vec.toList.traverse(interpret).map(_.toIndexedSeq).map(LvVect(_, size))
    case LvFunc(ByName(name)) => getFunc(name).map(_.map(LvFunc).getOrElse(LvUndefined))
      .flatMap(unravel)
    case f@LvFunc(_) => Applicative[Unravel].pure(f)
  }

  def interpret(expr: LvExpression): Unravel[LvObject] = Reader(env =>
    interpretCall(expr, IndexedSeq.empty, IndexedSeq.empty, env).value)

  def interpretCall(expr: LvExpression, parameters: IndexedSeq[LvObject], capture: IndexedSeq[LvObject], env: LvEnvironment): Eval[LvObject] = {

    def foldArgs(argV: IndexedSeq[LvExpression]): Eval[IndexedSeq[LvObject]] =
      argV.toList.foldLeftM(IndexedSeq.empty[LvObject]) { case (as, a) => interpretCall(a, parameters, capture, env).map(as :+ _) }

    expr match {
      case LvParameter(i) if i.value < parameters.size => Eval.now(parameters(i.value))
      case LvParameter(i) if i.value >= parameters.size => Eval.now(capture(i.value - parameters.size))
      case LvLiteral(lit) => Eval.now(lit)
      case LvCall(ByCode(code, arity), argV) if argV.size == arity =>
        foldArgs(argV).flatMap(args => interpretCall(code, args, parameters ++ capture, env))
      case LvCall(ByName(name), argV) =>
        interpretCall(LvCall(env.lvFuncs(name), argV), parameters, capture, env)
      case LvCall(ByNative(name), argV) =>
        foldArgs(argV).map(env.nativeFuncs(name))

    }
  }
}
