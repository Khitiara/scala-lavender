package lavender.parse

import enumeratum._

import scala.collection.immutable

sealed trait TokenType extends EnumEntry

case object TokenType extends Enum[TokenType] with CatsEnum[TokenType] {
  case object IDENT extends TokenType
  case object QUAL_IDENT extends TokenType
  case object FLOATING extends TokenType
  case object INTEGER extends TokenType
  case object SYMBOL extends TokenType
  case object QUAL_SYMBOL extends TokenType
  case object FUNC_SYMBOL extends TokenType
  case object FUNC_VAL extends TokenType
  case object QUAL_FUNC_VAL extends TokenType
  case object STRING extends TokenType
  case object ELLIPSES extends TokenType
  case object EMPTY_ARGS extends TokenType
  case object LITERAL extends TokenType

  override val values: immutable.IndexedSeq[TokenType] = findValues
}

case class Token(value: String, tokenType: TokenType) {
  override def toString: String = s"Tok($value, ${tokenType.entryName})"
}
