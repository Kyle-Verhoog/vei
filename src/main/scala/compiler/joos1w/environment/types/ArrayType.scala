package compiler.joos1w.environment.types
import scala.Boolean

class ArrayType(rootType: AbstractType) extends AbstractType {
  override def stringType: String = {
    rootType.stringType + "[]"
  }

  override def isNumeric: Boolean = {
    rootType.isNumeric
  }
}
