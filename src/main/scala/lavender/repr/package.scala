package lavender

import lavender.expr.LvExpression

import scala.collection.immutable

package object repr {

  sealed trait LvObject

  case class LvString(str: String) extends LvObject

  case class LvVect(vec: immutable.IndexedSeq[LvExpression], size: Int) extends LvObject

  case class LvFunc(fun: LvFunctionHandle) extends LvObject

  case object LvUndefined extends LvObject

  case class LvInt(int: BigInt) extends LvObject

  case class LvFloat(float: BigDecimal) extends LvObject
}
