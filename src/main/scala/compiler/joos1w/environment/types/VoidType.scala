package compiler.joos1w.environment.types
import scala.Boolean

class VoidType extends AbstractType {
  override def stringType: String = "void"

  override def isVoid: Boolean = true
}
