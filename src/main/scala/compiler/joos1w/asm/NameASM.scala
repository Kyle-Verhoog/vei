package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._

object NameASM {
  def nameASM(ast: Option[AST], lvalue: Boolean): ASM = {
    ast match {
      case Some(name: Name) =>
        var asm = ASM("")

        val resolvedParts =
          Joos1WCodeGen.resolveQualifiedName(name.name, name.env)

        resolvedParts.zipWithIndex.foreach({
          case (lEnv: LengthEnvironmentt, _) =>
            asm = asm ++ ASM(s""";; array length field lookup
                   |;; assume array start in eax, length of array stored at position 0
                   |mov eax, [eax] ;; eax <- length of array
                   |""".stripMargin)
          case (vEnv: VariableEnvironment, i) =>
            vEnv.myAst match {
              case field: FieldDeclaration =>
                if (field.modifiers.contains("static")) {
                  val fieldLabel = Joos1WCodeGen.staticFieldLabel(vEnv)
                  // if this is the first occurrence of a static field then
                  // load the implicit class reference
                  if (i == 0) {
                    val clsEnv = vEnv.findEnclosingClass()
                    val clsLabel = Joos1WCodeGen.classLabel(clsEnv)
                    asm = asm ++ ASM(
                      s"mov eax, $clsLabel ;; load implicit class address for static field")
                  }

                  if (lvalue) {
                    asm = asm ++
                      ASM(s"""
                         |;; l-value static field lookup
                         |mov eax, $fieldLabel ;; eax <- address of static field (on the stack, if applicable)
                      """.stripMargin)
                  } else {
                    asm = asm ++
                      ASM(s"""
                           |;; r-value static field lookup
                           |mov eax, $fieldLabel ;; eax <- address of static field (on the stack, if applicable)
                           |mov eax, [eax]       ;; eax <- value of static field (addr for objs, literal for prims)
                      """.stripMargin)
                  }
                } else {
                  // if this is the first occurrence of an instance field then
                  // load the implicit this obj reference
                  if (i == 0) {
                    val methodEnv = name.env.findEnclosingMethod()
                    val offset = 4 * methodEnv.paramCount
                    asm = asm ++
                      ASM(s"""
                             |mov eax, [ebp + $offset] ;; eax <- address of implicit "this" obj reference
           """.stripMargin)
                  }

                  val fieldOffset = 4 * (vEnv.order + 1)
                  if (lvalue) {
                    asm = asm ++
                      ASM(s"""
                             |;; l-value instance field access
                             |;; assume eax has address of object for field
                             |add eax, $fieldOffset ;; eax <- addr of ${field.name}
                             |""".stripMargin)
                  } else {
                    asm = asm ++
                      ASM(s"""
                             |;; r-value instance field access
                             |;; assume eax has address of object for field
                             |add eax, $fieldOffset ;; eax <- addr of ${field.name}
                             |mov eax, [eax] ;; eax <- ${field.name} (addr for obj, literal for prim)
                             |""".stripMargin)
                  }
                }
              case lvar: LocalVariableDeclaration =>
                val offset = 4 * vEnv.localVarOffset
                if (lvalue) {
                  asm = asm ++
                    ASM(s"""
                           |;; l-value local variable lookup "${lvar.name}"
                           |mov eax, ebp     ;; eax <- frame pointer
                           |sub eax, $offset ;; eax <- addr of local variable on stack
                           | """.stripMargin)
                } else {
                  asm = asm ++
                    ASM(s"""
                           |;; r-value local variable lookup "${lvar.name}"
                           |mov eax, ebp     ;; eax <- addr of local variable on stack
                           |sub eax, $offset
                           |mov eax, [ebx]   ;; eax <- value of local variable (addr for obj, literal for prim)
                           | """.stripMargin)
                }
              case fparam: FormalParameter =>
                val offset = 4 * vEnv.fpOffset

                if (lvalue) {
                  asm = asm ++
                    ASM(s"""
                           |;; l-value formal param lookup "${fparam.name}"
                           |mov eax, ebp     ;; eax <- address of param "${fparam.name}"
                           |add eax, $offset
                           | """.stripMargin)
                } else {
                  asm = asm ++
                    ASM(s"""
                           |;; r-value formal param lookup "${fparam.name}"
                           |mov eax, ebp     ;; eax <- address of param "${fparam.name}"
                           |add eax, $offset
                           |mov eax, [ebx]   ;; eax <- value of param "${fparam.name}" (addr for obj, literal for prim)
                           | """.stripMargin)
                }
              case x =>
                asm = asm ++ ASM(
                  s";; TODO? resolveQualifiedName gave ${x} for ${name.name}")
            }
          case (clsEnv: ClassEnvironment, _) =>
            val label = Joos1WCodeGen.classLabel(clsEnv)
            asm = asm ++ ASM(s"mov eax, $label ;; eax <- class reference")
          case _ =>
            asm = asm ++ ASM(s";; TODO $name codegen")
        })
        asm
      case _ => throw new MatchError(s"fall through when handling name")
    }
  }
}
