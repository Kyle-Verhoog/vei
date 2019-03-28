package compiler.joos1w.asm

import compiler.joos1w.ast._

object StatementASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def topLevelIfStatementASM(expr: TopLevelIf): ASM = {
    val myCounter = incrementAndReturnCounter
    val children = expr.children
    ifStatementChildrenASM(children)
  }

  def ifStatementChildrenASM(children: List[AST]): ASM = {
    if (children.isEmpty) return ASM("")

    children.head match {
      case child: IfStatement =>
        val myCounter = incrementAndReturnCounter
        val conditionCode = MethodASM.methodASM(Some(child.expr)).code
        val bodyCode = MethodASM.methodASM(Some(child.body)).code
        val elseCode = ifStatementChildrenASM(children.tail).code

        ASM(s"""
               |;; IF ( ${child.expr} )
               |$conditionCode
               |cmp eax, 0
               |je .else${myCounter}
               |${bodyCode}
               |jmp .endif${myCounter}
               |.else${myCounter}:
               |${elseCode}
               |.endif${myCounter}:
               |""".stripMargin)
      case child: TopLevelIf =>
        topLevelIfStatementASM(child)
      case child =>
        if (children.length > 1)
          throw new RuntimeException(
            "Encountered non if statement child in the middle of an if statement list")
        MethodASM.methodASM(Some(child))
    }
  }

}
