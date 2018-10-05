package lavender

import lavender.expr.LvExpression

import scala.collection.immutable

package object repr {

  sealed abstract class LvObject(val typ: Int)

  case class LvString(str: String) extends LvObject(2)

  case class LvVect(vec: immutable.IndexedSeq[LvExpression], size: Int) extends LvObject(3)

  case class LvFunc(fun: LvFunctionHandle) extends LvObject(4)

  case class LvInt(int: BigInt) extends LvObject(5)

  case class LvFloat(float: Double) extends LvObject(1)

  case object LvUndefined extends LvObject(0)

}
