package compiler.joos1w.environment.types

import compiler.joos1w.environment.ClassEnvironment

class CustomType(val env: ClassEnvironment) extends AbstractType {
  override def stringType: String = env.qualifiedName

  override def isSubClassOf(ttype: AbstractType): Boolean = {
    println("checking sub class")
    println("my super " + env.superSet)
    ttype match {
      case ttype: CustomType => env.isSubClassOf(ttype.env)
      case ttype: StringType => env.isSubClassOf(ttype.env)
      case _                 => false
    }
  }

  override def toString: String = {
    "CustomType(name: " + stringType + ")"
  }
}
