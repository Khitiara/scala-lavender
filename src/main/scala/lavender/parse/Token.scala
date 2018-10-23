package lavender.parse

import enumeratum._

import scala.collection.immutable

sealed trait TokenType extends EnumEntry

case object TokenType extends Enum[TokenType] with CatsEnum[TokenType] {
  object IDENT extends TokenType
  object QUAL_IDENT extends TokenType
  object FLOATING extends TokenType
  object INTEGER extends TokenType
  object SYMBOL extends TokenType
  object QUAL_SYMBOL extends TokenType
  object FUNC_SYMBOL extends TokenType
  object FUNC_VAL extends TokenType
  object QUAL_FUNC_VAL extends TokenType
  object STRING extends TokenType
  object ELLIPSES extends TokenType
  object EMPTY_ARGS extends TokenType

  override val values: immutable.IndexedSeq[TokenType] = findValues
}

case class Token(value: String, tokenType: TokenType) {

}
