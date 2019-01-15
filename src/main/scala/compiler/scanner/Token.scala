package compiler.scanner

@SerialVersionUID(100L)
class Token(val tokenType: String, var value: String) extends Serializable {
  override def toString = {
    s"""TOKEN(TYPE : $tokenType VALUE: $value)"""
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Token =>
        tokenType.equals(that.tokenType) && value.equals(that.value)
      case _ => false
    }
}
