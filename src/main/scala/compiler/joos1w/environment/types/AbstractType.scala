package compiler.joos1w.environment.types

abstract class AbstractType {
  def isNumeric: Boolean = false

  def isVoid: Boolean = false

  def isString: Boolean = false

  def stringType: String

  def equals(obj: AbstractType): Boolean = {
    this.getClass == obj.getClass
  }
}
