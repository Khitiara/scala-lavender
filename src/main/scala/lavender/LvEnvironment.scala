package lavender

import lavender.repr.LvFunctionHandle
import lavender.util.SnowflakeMap

case class LvEnvironment(lvFuncs: SnowflakeMap[FunctionTag, LvFunctionHandle], nativeFuncs: SnowflakeMap[FunctionTag, LvNativeFunc])
