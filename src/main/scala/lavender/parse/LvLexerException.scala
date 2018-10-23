package lavender.parse

case class LvLexerException($msg: String) extends Exception($msg)

case class LvParserException($msg: String) extends Exception($msg)
