package lavender

import lavender.repr.LvFunctionHandle
import lavender.util.SnowflakeMap
import monocle.macros.Lenses

@Lenses
case class LvEnvironment(lvFuncs: SnowflakeMap[FunctionTag, LvFunctionHandle],
                         nativeFuncs: SnowflakeMap[FunctionTag, LvNativeFunc],
                         lp: LvSourcePath)


