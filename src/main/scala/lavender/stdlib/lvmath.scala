package lavender.stdlib

import lavender._
import lavender.repr.{LvFloat, LvInt, LvObject, LvUndefined}

import math._

object lvmath {

  def _sin: LvNativeFunc = lift1(sin)

  def _cos: LvNativeFunc = lift1(cos)

  def _tan: LvNativeFunc = lift1(tan)

  def _asin: LvNativeFunc = lift1(asin)

  def _acos: LvNativeFunc = lift1(acos)

  def _atan: LvNativeFunc = lift1(atan)

  def _atan2: LvNativeFunc = lift2(atan2)

  def lift2(f: (Double, Double) => Double): LvNativeFunc1 = args => (args(0), args(1)) match {
    case (UnwrapNum(a), UnwrapNum(b)) => LvFloat(f(a, b))
    case _ => LvUndefined
  }

  def _sinh: LvNativeFunc = lift1(sinh)

  def lift1(f: Double => Double): LvNativeFunc1 = args => args(0) match {
    case UnwrapNum(x) => LvFloat(f(x))
    case _ => LvUndefined
  }

  def _cosh: LvNativeFunc = lift1(cosh)

  def _tanh: LvNativeFunc = lift1(tanh)

  def _exp: LvNativeFunc = lift1(exp)

  def _log: LvNativeFunc = lift1(log)

  def _log10: LvNativeFunc = lift1(log10)

  def _sqrt: LvNativeFunc = lift1(sqrt)

  def _ceil: LvNativeFunc = lift1(ceil)

  def _floor: LvNativeFunc = lift1(floor)

  def _round: LvNativeFunc = lift1(round)

  def _abs(args: LvObject*): LvObject = args(0) match {
    case LvFloat(f) => LvFloat(abs(f))
    case LvInt(i) => LvInt(i.abs)
    case _ => LvUndefined
  }

  def _sgn(args: LvObject*): LvObject = args(0) match {
    case LvFloat(f) => if (f < 0) LvInt(-1) else if (f == 0) LvInt(0) else LvInt(1)
    case LvInt(i) => if (i < 0) LvInt(-1) else if (i == 0) LvInt(0) else LvInt(1)
  }

  case object UnwrapNum {
    def unapply(arg: LvObject): Option[Double] = arg match {
      case LvInt(i) => Some(i.doubleValue())
      case LvFloat(f) => Some(f)
      case _ => None
    }
  }

}
