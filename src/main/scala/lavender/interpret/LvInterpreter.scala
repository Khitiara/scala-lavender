package lavender.interpret

import cats.data.Reader
import cats.{Applicative, Eval}
import cats.implicits._
import lavender._
import lavender.expr._
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

  def interpret(expr: LvExpression): Unravel[LvObject] = Reader(interpretCall(expr, IndexedSeq.empty, IndexedSeq.empty, _).value)

  private var anonFnCounter: Int = 0

  def anonFunctionName(): FunctionName = {
    anonFnCounter += 1
    FunctionName(s"anon$$$anonFnCounter")
  }

  /**
    * A tree visitor running on a trampoline, this method recursively interprets lavender expressions
    *
    * @param expr       The expression to evaluate
    * @param parameters The expressions of parameters of the current method invocation
    * @param capture    The expressions of any captured information
    * @param env        The object representing the environment in which we interpret.
    *                   Used for fetching function declarations for function calls
    * @return A trampoline
    */
  private def interpretCall(expr: LvExpression, parameters: IndexedSeq[LvExpression], capture: IndexedSeq[LvExpression], env: LvEnvironment): Eval[LvObject] = {

    def foldArgs(argV: IndexedSeq[LvExpression]): Eval[IndexedSeq[LvObject]] =
      argV.toList.traverse(interpretCall(_, parameters, capture, env)).map(_.toIndexedSeq)

    expr match {
      case LvParameter(i) if i.value < parameters.size => Eval.defer(interpretCall(parameters(i.value), parameters, capture, env))
      case LvParameter(i) if i.value >= parameters.size => Eval.defer(interpretCall(capture(i.value - parameters.size), parameters, capture, env))
      case LvLiteral(lit) => Eval.now(lit)
      case LvCall(ByCode(_, code, arity, cap), argV) if argV.size == arity => interpretCall(code, argV, cap, env)
      case LvCall(ByName(name, arity), argV) if argV.size == arity =>
        interpretCall(LvCall(env.lvFuncs(name), argV), parameters, capture, env)
      case LvCall(ByNative(name, arity, cap), argV) if argV.size == arity =>
        foldArgs(argV ++ cap).map(_.toArray).map(env.nativeFuncs(name)).flatMap(interpretCall(_, parameters, capture, env))
      case LvDecl(code, name, arity) =>
        Eval.now(LvFunc(ByCode(name.getOrElse(anonFunctionName()), code, arity, parameters ++ capture)))
    }
  }
}
