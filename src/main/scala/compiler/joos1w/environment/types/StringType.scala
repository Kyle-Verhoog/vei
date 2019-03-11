package compiler.joos1w.environment.types
import compiler.joos1w.environment.{ClassEnvironment, GenericEnvironment}

class StringType(ev: GenericEnvironment) extends AbstractType {
  val env: ClassEnvironment = ev.serarchForClass("java.lang.String").get

  override def isSubClassOf(ttype: AbstractType): Boolean = {
    ttype match {
      case ttype: CustomType => env.isSubClassOf(ttype.env)
      case ttype: StringType => env.isSubClassOf(ttype.env)
      case _                 => false
    }
  }

  override def isString: Boolean = true

  override def stringType: String = "java.lang.String"
}
