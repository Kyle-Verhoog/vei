package compiler.joos1w.environment.types
import scala.Boolean

class StringType extends AbstractType {
  override def isString: Boolean = true

  override def stringType: String = "String"
}
