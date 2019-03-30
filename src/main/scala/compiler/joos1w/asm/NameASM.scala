package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._

object NameASM {
  def nameASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(name: Name) =>
        if (name.name.contains(".")) {
          ASM(
            s";; TODO field/qualified name lookup - could not find ${name.name}")
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
      case _ => throw new RuntimeException(s"NameASM match error $ast")
    }
  }
}
