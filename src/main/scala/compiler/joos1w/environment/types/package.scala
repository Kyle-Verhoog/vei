package compiler.joos1w.environment

import compiler.joos1w.environment.types.numeric.{
  BytesType,
  CharType,
  IntType,
  ShortType
}
import exceptions.EnvironmentError

package object types {
  def buildTypeFromString(ttype: String,
                          env: GenericEnvironment): AbstractType = {
    //println("building type " + ttype)
    if (ttype.length >= 3 && ttype.takeRight(2) == "[]") {
      return new ArrayType(buildTypeFromString(ttype.dropRight(2), env))
    }

    ttype match {
      case "int"     => new IntType
      case "byte"   => new BytesType
      case "short"   => new ShortType
      case "char"    => new CharType
      case "String"  => new StringType(env)
      case "boolean" => new BooleanType
      case "void"    => new VoidType
      case "null"    => new NullType
      case _ => {
        env match {
          case env: ClassEnvironment =>
            new CustomType(env)
          case _ =>
            val classEnv = env.serarchForClass(ttype)
            if (classEnv.isEmpty) {
              throw EnvironmentError(
                s"Type: $ttype does not resolve to a class name!")
            }
            new CustomType(classEnv.get)
        }
      }
    }
  }
}
