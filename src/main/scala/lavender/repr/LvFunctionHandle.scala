package lavender.repr

import lavender.FunctionName
import lavender.expr.LvExpression

sealed trait LvFunctionHandle

object LvFunctionHandle {
  case class ByName(name: FunctionName) extends LvFunctionHandle
  case class ByCode(code: LvExpression, arity: Int) extends LvFunctionHandle
  case class ByNative(name: FunctionName) extends LvFunctionHandle
}
