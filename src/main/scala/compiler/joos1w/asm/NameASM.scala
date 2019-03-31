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
            case vEnv: VariableEnvironment =>
              vEnv.myAst match {
                case field: FieldDeclaration =>
                  if (field.modifiers.contains("static")) {
                    val cls = vEnv.findEnclosingClass()
                    val fieldLabel = Joos1WCodeGen.staticFieldLabel(vEnv)
                    asm = asm ++ ASM(s"""
                        |;; static field lookup
                        |mov ebx, $fieldLabel ;; ebx <- address of static field
                        |mov eax, [ebx]       ;; eax <- value of static field
                      """.stripMargin)
                  } else {
                    // val fieldOffset =
                    asm = asm ++ ASM(s"""
                         |;; TODO field code gen
                         |;; assume eax has address of object for field
                         |""".stripMargin)
                  }
                case lvar: LocalVariableDeclaration =>
                  val offset = 4 * vEnv.localVarOffset
                  asm = asm ++ ASM(s"""
                         |;; local variable lookup
                         |mov ebx, ebp     ;; ebx <- address of local variable
                         |sub ebx, $offset
                         |mov eax, [ebx]   ;; eax <- value of local variable
                         | """.stripMargin)
                case fparam: FormalParameter =>
                  val offset = 4 * vEnv.fpOffset
                  asm = asm ++ ASM(s"""
                                      |;; formal param lookup
                                      |mov ebx, ebp     ;; ebx <- address of local variable
                                      |add ebx, $offset
                                      |mov eax, [ebx]   ;; eax <- value of local variable
                                      | """.stripMargin)
                case x => asm = asm ++ ASM(s";; TODO? ${x}")
              }
            case clsEnv: ClassEnvironment =>
              asm = asm ++ ASM(s";; TODO cls name codegen")
            case _ =>
              asm = asm ++ ASM(s";; TODO $name codegen")
          })
        asm
      /*
        if (name.name.contains(".")) {
          ASM(
            s";; TODO field/qualified name lookup - could not find basename ${name.name}")
        } else {
          name.env.serarchForVariable(name.name) match {
            case Some(v: VariableEnvironment) =>
              v.myAst match {
                case field: FieldDeclaration =>
                  if (field.modifiers.contains("static")) {
                    val fieldLabel = Joos1WCodeGen.staticFieldLabel(
                      field.env.asInstanceOf[VariableEnvironment])
                    ASM(
                      s"""mov ebx, $fieldLabel     ;; ebx := &field ${name.name}
                         | mov eax, [ebx] ;; eax := *field ${name.name}
                         | """.stripMargin)
                  } else {
                    ASM(s";; TODO non-static fieldssss")
                  }
                case localVar: LocalVariableDeclaration =>
                  if (v.offset.isDefined) {
                    val offset = v.offset.get * 4
                    ASM(s"""mov ebx, ebp     ;; ebx := &var ${name.name}
                         |  sub ebx, $offset
                         |  mov eax, [ebx] ;; eax := *var ${name.name}
                         |  """.stripMargin)
                  } else {
                    throw new RuntimeException(s"OFFSET NOT DEFINED")
                  }
                case formalParameter: FormalParameter =>
                  ASM(s"""
                       |;; TODO formal parameters
                     """.stripMargin)
                case _ =>
                  throw new RuntimeException(
                    s"NameASM match error VariableEnv ${v.myAst}")
              }
            case None =>
              ASM(
                s";; TODO field/qualified name lookup - could not find ${name.name}")
          }
        }
       */
      case _ => throw new RuntimeException(s"NameASM match error $ast")
    }
  }
}
