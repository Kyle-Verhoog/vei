package compiler.joos1w.env

abstract class Type {
  def toStrTree: String
}

// abstract class UserDefinedType extends Type {}
//
// class PrimitiveType extends Type {}
//
// class ClassType extends UserDefinedType {}
//
// class InterfaceType extends UserDefinedType {}
//
// class IntType(ctx: Context = null) extends Primitive("int", ctx) {}
// class BooleanType(ctx: Context = null) extends Primitive("boolean", ctx) {}
// class StringType(ctx: Context = null)
//   extends Primitive("java.lang.String", ctx) {}
// class CharType(ctx: Context = null) extends Primitive("char", ctx) {}
// class ByteType(ctx: Context = null) extends Primitive("byte", ctx) {}
// class NullType(ctx: Context = null) extends Primitive("null", ctx) {}
// class VoidType(ctx: Context = null) extends Primitive("void", ctx) {}
// class ShortType(ctx: Context = null) extends Primitive("short", ctx) {}
