package lavender.interpret

import cats.Eval
import cats.data.Reader
import cats.implicits._
import lavender._
import lavender.expr._
import lavender.repr.LvFunctionHandle.{ByCode, ByName, ByNative}
import lavender.repr._

/**
  * Interpreter for lavender expressions.
  * Maintains an internal counter used for giving a name to anonymous function declarations
  */
class LvInterpreter {
  /**
    * A tree visitor running on a trampoline, this method recursively interprets lavender expressions.
    *
    * @param expr the [[LvExpression]] to interpret
    * @return A reader monad which represents the act of interpreting this expression with an arbitrary [[LvEnvironment]]
    */
  def trace(expr: LvExpression): Repl[LvObject] = Repl.read(interpretCall(expr, IndexedSeq.empty, IndexedSeq.empty, _).value)

  /**
    * Shortcut for [[trace]]ing a function call, created at this time
    *
    * @param fun  A handle to the function to be called
    * @param args The arguments to be passed to the function
    * @param post A processor to convert the output object
    * @param env  The environment in which to run this
    * @tparam A The desided type of the java output
    * @return The result of the lavender function call
    */
  def traceCall[A](fun: LvFunctionHandle, args: LvExpression*)(post: LvObject => A)(env: LvEnvironment): A = {
    if (args.length != fun.arity)
      throw LvInterpreterException("Invalid arity for function called by native code")
    trace(LvCall(fun, args.toIndexedSeq)).map(post).run(env).unsafeRunSync()._2
  }


  private var anonFnCounter: Int = 0

  private def anonFunctionName(): FunctionName = {
    anonFnCounter += 1
    FunctionName(s"anon$$$anonFnCounter")
  }

  /**
    * A tree visitor running on a trampoline, this method recursively interprets lavender expressions
    *
    * @param expr       The [[LvExpression]] to evaluate
    * @param parameters The expressions of parameters of the current method invocation
    * @param capture    The expressions of any captured information
    * @param env        The object representing the environment in which we interpret.
    *                   Used for fetching function declarations for function calls
    * @return A trampoline which, when unwound, evaluates to an [[LvObject]]
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
      case LvGuard(values) =>
        values.findM(v => interpretCall(v._1, parameters, capture, env).map(stdlib.toBool))
          .map(_.toRight(LvInterpreterException("Match error!")).toTry.get)
          .flatMap(v => interpretCall(v._2, parameters, capture, env))
    }
  }
}
