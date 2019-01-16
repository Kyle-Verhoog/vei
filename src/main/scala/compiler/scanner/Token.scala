package compiler.scanner

@SerialVersionUID(100L)
class Token(val tokenType: String, var value: String, val tokenNumber: Int = 0) extends Serializable {
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
