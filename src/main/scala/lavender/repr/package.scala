package lavender

import scala.collection.immutable

package object repr {
  sealed trait LvObject

  case class LvString(str: String) extends LvObject
  case class LvVect(vec: immutable.IndexedSeq[LvObject], size: Int) extends LvObject
  case class LvFunc(fun: LvFunctionHandle) extends LvObject
  case object Undefined extends LvObject
  case class LvInt(int: BigInt) extends LvObject
  case class LvFloat(float: BigDecimal) extends LvObject
}
