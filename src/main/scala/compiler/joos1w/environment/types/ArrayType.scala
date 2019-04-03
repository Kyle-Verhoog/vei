package compiler.joos1w.environment.types

case class ArrayType(rootType: AbstractType) extends AbstractType {
  override def stringType: String = {
    rootType.stringType + "[]"
  }

  override def isNumeric: Boolean = {
    rootType.isNumeric
  }

  override def toString: String = {
    "ArrayType(" + stringType + ")"
  }
}
