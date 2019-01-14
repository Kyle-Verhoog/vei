package compiler.scanner

class Token(val tokenType: String, val value: String) {
  override def toString = {
    s"""TOKEN(TYPE : $tokenType VALUE: $value)"""
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Token => tokenType.equals(that.tokenType) && value.equals(that.value)
      case _ => false
    }
}
