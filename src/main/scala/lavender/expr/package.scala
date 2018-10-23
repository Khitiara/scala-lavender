package lavender

import cats.free.Free
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import lavender.repr.{LvFunctionHandle, LvObject}

import scala.collection.immutable

package object expr {

//  sealed trait LvExpression
//
//  case class LvParameter(i: Int Refined NonNegative) extends LvExpression
//
//  case class LvCall(i: Int Refined NonNegative) extends LvExpression
//
//  case class LvCapture(inner: LvFunctionHandle, cap: immutable.IndexedSeq[LvObject]) extends LvExpression
//
//  case class LvLiteral(value: LvObject) extends LvExpression
//
//
//  type ExprStack = List[LvExpression]
  sealed trait LvExpression
  case class LvCall(ptr: LvFunctionHandle, argV: IndexedSeq[LvExpression]) extends LvExpression
  case class LvLiteral(lit:  LvObject) extends LvExpression
  case class LvParameter(i: Int Refined NonNegative) extends LvExpression
  case class LvDecl(code: LvExpression, name: Option[FunctionName], arity: Int) extends LvExpression
}
