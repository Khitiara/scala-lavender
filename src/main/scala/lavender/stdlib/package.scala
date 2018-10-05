package lavender

import java.math.BigInteger

import lavender.expr.{LvCall, LvExpression, LvLiteral}
import lavender.interpret.LvInterpreter
import lavender.repr._

import scala.collection.immutable
import scala.util.Try

package object stdlib {
  def complete(f: PartialFunction[Array[LvObject], LvObject])(args: Array[LvObject]): LvObject =
    f.applyOrElse(args, _ => LvUndefined)


  def defined(args: LvObject*): LvObject = LvInt(if (args(0) != LvUndefined) 1 else 0)

  def undefined(args: LvObject*): LvObject = LvUndefined

  def typeof(args: LvObject*): LvObject = LvInt(args(0).typ)

  def cat(args: LvObject*): LvObject = {
    val v = args.flatMap {
      case LvVect(vec, _) => vec
      case o => IndexedSeq(LvLiteral(o))
    }.toIndexedSeq
    LvVect(v, v.size)
  }

  def call(args: LvObject*): LvExpression = args(0) match {
    case LvFunc(fun) => args(1) match {
      case LvVect(vec, _) =>
        LvCall(fun, vec)
      case _ => LvLiteral(LvUndefined)
    }
    case _ => LvLiteral(LvUndefined)
  }

  def toStr(o: LvObject)(implicit lvInterpreter: LvInterpreter, environment: LvEnvironment): String = o match {
    case LvString(s) => s
    case LvUndefined => "<undefined>"
    case LvFloat(float) => float.toString
    case LvInt(int) => int.toString()
    case LvFunc(fun) => ???
    case LvVect(vec, _) => vec.map(lvInterpreter.interpret)
      .map(_.run(environment)).map(toStr).mkString("{ ", ", ", " }")
  }


  def str(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject =
    LvString(toStr(args(0))(lvInterpreter, environment))

  def num(args: LvObject*): LvObject = args(0) match {
    case f@LvFloat(_) => f
    case LvInt(int) => LvFloat(int.doubleValue())
    case LvString(str) => Try(LvFloat(str.toDouble)).recover { case _: NumberFormatException => LvUndefined }.get
    case _ => LvUndefined
  }

  def int(args: LvObject*): LvObject = args(0) match {
    case i@LvInt(_) => i
    case LvFloat(float) => LvInt(BigDecimal(float).toBigInt())
    case LvString(str) => Try(LvInt(BigInt(str))).recover { case _: NumberFormatException => LvUndefined }.get
    case _ => LvUndefined
  }

  def bool(args: LvObject*): LvObject = fromBool(toBool(args(0)))

  def fromBool(boolean: Boolean): LvObject = LvInt(if (boolean) 1 else 0)

  def len(args: LvObject*): LvObject = args(0) match {
    case LvString(str) => LvInt(str.length)
    case LvFunc(fun) => LvInt(fun.arity)
    case LvVect(_, size) => LvInt(size)
    case _ => LvUndefined
  }

  def equal(a: LvObject, b: LvObject)(implicit lvInterpreter: LvInterpreter, environment: LvEnvironment): Boolean = (a, b) match {
    case (LvUndefined, LvUndefined) => true
    case (LvFloat(fa), LvFloat(fb)) => fa == fb
    case (LvInt(ia), LvInt(ib)) => ia == ib
    case (LvString(sa), LvString(sb)) => sa == sb
    case (LvFunc(funa), LvFunc(funb)) => funa == funb // TODO: This probably doesnt work
    case (LvVect(va, sa), LvVect(vb, sb)) => sa == sb && va.map(lvInterpreter.interpret).map(_.run(environment))
      .zip(vb.map(lvInterpreter.interpret).map(_.run(environment))).forall { case (x, y) => equal(x, y) } // This is so dumb, sorry
    case _ => false
  }

  def eq(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject =
    fromBool(equal(args(0), args(1))(lvInterpreter, environment))

  def ltImpl(a: LvObject, b: LvObject)(implicit lvInterpreter: LvInterpreter, environment: LvEnvironment): Boolean = (a, b) match {
    case (LvUndefined, LvUndefined) => false
    case (LvFloat(x), LvFloat(y)) => x < y
    case (LvInt(x), LvInt(y)) => x < y
    case (LvString(x), LvString(y)) => x < y
    case (LvVect(xv, xs), LvVect(yv, ys)) => if (xs == ys) {
      for (i <- 0 until xs) {
        val x = lvInterpreter.interpret(xv(i)).run(environment)
        val y = lvInterpreter.interpret(yv(i)).run(environment)
        if (!equal(x, y))
          return ltImpl(x, y)
      }
      false
    } else xs < ys
    case (LvFunc(x), LvFunc(y)) => x.hashCode() < y.hashCode()
    case _ => a.typ < b.typ
  }

  def lt(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject =
    fromBool(ltImpl(args(0), args(1))(lvInterpreter, environment))

  def ge(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject =
    (args(0), args(1)) match {
      case (LvFloat(x), LvFloat(y)) if x.isNaN || y.isNaN => fromBool(false)
      case (a, b) => fromBool(!ltImpl(a, b)(lvInterpreter, environment))
    }

  def add(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x + y)
    case (LvFloat(x), LvInt(y)) => LvFloat(x + y.doubleValue())
    case (LvInt(x), LvFloat(y)) => LvFloat(x.doubleValue() + y)
    case (LvInt(x), LvInt(y)) => LvInt(x + y)
    case _ => LvUndefined
  }

  def sub(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x - y)
    case (LvFloat(x), LvInt(y)) => LvFloat(x - y.doubleValue())
    case (LvInt(x), LvFloat(y)) => LvFloat(x.doubleValue() - y)
    case (LvInt(x), LvInt(y)) => LvInt(x - y)
    case _ => LvUndefined
  }

  def mul(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x * y)
    case (LvFloat(x), LvInt(y)) => LvFloat(x * y.doubleValue())
    case (LvInt(x), LvFloat(y)) => LvFloat(x.doubleValue() * y)
    case (LvInt(x), LvInt(y)) => LvInt(x * y)
    case _ => LvUndefined
  }

  def div_(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x / y)
    case (LvFloat(x), LvInt(y)) => LvFloat(x / y.doubleValue())
    case (LvInt(x), LvFloat(y)) => LvFloat(x.doubleValue() / y)
    case (LvInt(x), LvInt(y)) => LvFloat(x.doubleValue() / y.doubleValue())
    case _ => LvUndefined
  }

  def idiv(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x / y)
    case (LvFloat(x), LvInt(y)) => doidivd(x, y.doubleValue())
    case (LvInt(x), LvFloat(y)) => doidivd(x.doubleValue(), y)
    case (LvInt(x), LvInt(y)) => LvInt(x / y)
    case _ => LvUndefined
  }

  def doidivd(a: Double, b: Double): LvObject = {
    if (a.isInfinite || b.isInfinite || b == 0)
      return LvUndefined
    LvInt(BigDecimal((a - a % b) / b).toBigInt())
  }

  def rem(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(x % y)
    case (LvFloat(x), LvInt(y)) => LvFloat(x % y.doubleValue())
    case (LvInt(x), LvFloat(y)) => LvFloat(x.doubleValue() % y)
    case (LvInt(x), LvInt(y)) => LvInt(x % y)
    case _ => LvUndefined
  }

  def pow(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvFloat(x), LvFloat(y)) => LvFloat(math.pow(x, y))
    case (LvFloat(x), LvInt(y)) => LvFloat(math.pow(x, y.doubleValue()))
    case (LvInt(x), LvFloat(y)) => LvFloat(math.pow(x.doubleValue(), y))
    case (LvInt(x), LvInt(y)) => LvInt(x.pow(y.intValue()))
    case _ => LvUndefined
  }

  def pos(args: LvObject*): LvObject = args(0) match {
    case f@LvFloat(_) => f
    case i@LvInt(_) => i
    case _ => LvUndefined
  }

  def neg(args: LvObject*): LvObject = args(0) match {
    case LvFloat(float) => LvFloat(-float)
    case LvInt(int) => LvInt(-int)
    case _ => LvUndefined
  }

  def map(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvVect(vec, size), LvFunc(fun)) =>
      LvVect(vec.map { o => LvCall(fun, IndexedSeq(o)) }, size)
    case _ => LvUndefined
  }

  def filter(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvVect(vec, _), LvFunc(fun)) =>
      def run(o: LvExpression): Boolean =
        lvInterpreter.traceCall(fun, o)(toBool)(environment)

      val v = vec.filter(run)
      LvVect(v, v.size)
    case _ => LvUndefined
  }

  def toBool(o: LvObject): Boolean = o match {
    case LvUndefined => false
    case LvFloat(float) => float != 0.0
    case LvInt(int) => int != 0
    case LvString(str) => str != null && str.nonEmpty
    case LvVect(_, size) => size > 0
    case LvFunc(_) => true
  }

  def fold(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject = (args(0), args(1), args(2)) match {
    case (LvVect(vec, _), accum, LvFunc(fun)) =>
      def run(accum: LvObject, o: LvExpression): LvObject = lvInterpreter.traceCall(fun, LvLiteral(accum), o)(identity)(environment)

      vec.foldLeft(accum)(run)
    case _ => LvUndefined
  }

  def slice(args: LvObject*): LvObject = (args(0), args(1), args(2)) match {
    case (LvVect(vec, _), LvInt(start), LvInt(end)) =>
      if (start > end || start < 0 || end < 0)
        return LvUndefined
      LvVect(vec.slice(start.intValue(), end.intValue()), start.intValue() - end.intValue())
    case _ => LvUndefined
  }

  // Actually takeWhile
  def take(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvVect(vec, _), LvFunc(fun)) =>
      def run(o: LvExpression): Boolean =
        lvInterpreter.trace(LvCall(fun, IndexedSeq(o))).map(toBool).run(environment)

      val v = vec.takeWhile(run)
      LvVect(v, v.size)
    case _ => LvUndefined
  }

  def skip(lvInterpreter: LvInterpreter, environment: LvEnvironment)(args: LvObject*): LvObject = (args(0), args(1)) match {
    case (LvVect(vec, _), LvFunc(fun)) =>
      def run(o: LvExpression): Boolean =
        lvInterpreter.trace(LvCall(fun, IndexedSeq(o))).map(toBool).run(environment)

      val v = vec.dropWhile(run)
      LvVect(v, v.size)
  }
}
