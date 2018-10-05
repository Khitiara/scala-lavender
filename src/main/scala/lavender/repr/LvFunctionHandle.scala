package lavender.repr

import lavender.FunctionName
import lavender.expr.LvExpression

sealed abstract class LvFunctionHandle {
  val arity: Int
}

object LvFunctionHandle {

  case class ByName(name: FunctionName, arity: Int) extends LvFunctionHandle

  case class ByCode(code: LvExpression, arity: Int) extends LvFunctionHandle

  case class ByNative(name: FunctionName, arity: Int) extends LvFunctionHandle

}
