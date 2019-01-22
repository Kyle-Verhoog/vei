package compiler.scanner

object Token {
  def filterTokensByType(tokens: List[Token],
                         types: Set[String]): List[Token] = {
    tokens.filter(t => !(types contains t.tokenType))
  }
}

@SerialVersionUID(100L)
class Token(val tokenType: String,
            var value: String,
            val tokenNumber: Int = 0,
            val fileName: String = "",
            val lineNum: Integer = -1,
            val col: Integer = -1)
    extends Serializable {
  override def toString = {
    s"""TOKEN(TYPE : $tokenType VALUE: $value)\n"""
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Token =>
        tokenType.equals(that.tokenType) && value.equals(that.value)
      case _ => false
    }

}
