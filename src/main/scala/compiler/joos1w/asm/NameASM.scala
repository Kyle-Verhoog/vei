package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._

object NameASM {
  def nameASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(name: Name) =>
        var asm = ASM("")
        Joos1WCodeGen
          .resolveQualifiedName(name.name, name.env)
          .foreach({
            case lEnv: LengthEnvironmentt =>
              asm = asm ++ ASM(s""";; array length field lookup
                   |;; assume array start in ebx
                   |mov eax, [eax] ;; eax <- length of array
                   |""".stripMargin)
            case vEnv: VariableEnvironment =>
              vEnv.myAst match {
                case field: FieldDeclaration =>
                  if (field.modifiers.contains("static")) {
                    val fieldLabel = Joos1WCodeGen.staticFieldLabel(vEnv)
                    asm = asm ++ ASM(s"""
                        |;; static field lookup
                        |mov ebx, $fieldLabel ;; ebx <- address of static field
                        |mov eax, [ebx]       ;; eax <- value of static field
                      """.stripMargin)
                  } else {
                    val fieldOffset = 4 * (vEnv.order + 1)
                    asm = asm ++ ASM(s"""
                         |;; Instance field access
                         |;; assume ebx has address of object for field
                         |add ebx, $fieldOffset ;; ebx <- addr of ${field.name}
                         |mov eax, [ebx] ;; eax <- ${field.name}
                         |""".stripMargin)
                  }
                case lvar: LocalVariableDeclaration =>
                  val offset = 4 * vEnv.localVarOffset
                  asm = asm ++ ASM(s"""
                         |;; local variable lookup "${lvar.name}"
                         |mov ebx, ebp     ;; ebx <- address of local variable
                         |sub ebx, $offset
                         |mov eax, [ebx]   ;; eax <- value of local variable
                         | """.stripMargin)
                case fparam: FormalParameter =>
                  val offset = 4 * vEnv.fpOffset
                  asm = asm ++ ASM(s"""
                                      |;; formal param lookup "${fparam.name}"
                                      |mov ebx, ebp     ;; ebx <- address of param "${fparam.name}"
                                      |add ebx, $offset
                                      |mov eax, [ebx]   ;; eax <- value of param "${fparam.name}"
                                      | """.stripMargin)
                case x =>
                  asm = asm ++ ASM(
                    s";; TODO? resolveQualifiedName gave ${x} for ${name.name}")
              }
            case clsEnv: ClassEnvironment =>
              val label = Joos1WCodeGen.classLabel(clsEnv)
              asm = asm ++ ASM(s"mov ebx, ${label}")
            case _ =>
              asm = asm ++ ASM(s";; TODO $name codegen")
          })
        asm
    }
  }
}
