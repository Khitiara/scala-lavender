package lavender

import lavender.LvEnvironment.LvNativeFunc
import lavender.repr.{LvFunctionHandle, LvObject}
import lavender.util.SnowflakeMap

object LvEnvironment {
  type LvNativeFunc = Seq[LvObject] => LvObject
}

case class LvEnvironment(lvFuncs: SnowflakeMap[FunctionTag, LvFunctionHandle], nativeFuncs: SnowflakeMap[FunctionTag, LvNativeFunc])
