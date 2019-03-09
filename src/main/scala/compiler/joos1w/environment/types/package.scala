package compiler.joos1w.environment

import compiler.joos1w.environment.types.numeric.{
  BytesType,
  CharType,
  IntType,
  ShortType
}

package object types {
  def buildTypeFromString(ttype: String): AbstractType = {
    if (ttype.length > 3 && ttype.takeRight(2) == "[]") {
      return new ArrayType(buildTypeFromString(ttype.dropRight(2)))
    }

    ttype match {
      case "int"     => new IntType
      case "bytes"   => new BytesType
      case "short"   => new ShortType
      case "char"    => new CharType
      case "String"  => new StringType
      case "boolean" => new BooleanType
      case "void"    => new VoidType
      case _         => new CustomType(ttype)
    }
  }
}
