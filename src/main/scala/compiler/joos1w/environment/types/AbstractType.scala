package compiler.joos1w.environment.types

abstract class AbstractType {
  def isNumeric: Boolean = false

  def isVoid: Boolean = false

  def isString: Boolean = false

  def stringType: String

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: AbstractType => {
        println("comparing " + this + " with " + obj)
        this.stringType == obj.stringType
      }
    }
  }
}
