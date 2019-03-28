package compiler.joos1w.asm

import compiler.joos1w.ast._

object StatementASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def whileStatementASM(stmt: WhileStatement): ASM = {
    val myCounter = incrementAndReturnCounter
    val conditionCode = MethodASM.methodASM(Some(stmt.expr)).code
    val bodyCode = MethodASM.methodASM(Some(stmt.body)).code

    ASM(s"""
           |;; While ( ${stmt.expr} )
           |.start_while${myCounter}:
           |$conditionCode
           |cmp eax, 0
           |je .end_while${myCounter}
           |${bodyCode}
           |jmp .start_while${myCounter}
           |.end_while${myCounter}:
           |""".stripMargin)
  }

  def forStatementASM(stmt: ForStatement): ASM = {
    val myCounter = incrementAndReturnCounter

    val initializationCode = MethodASM.methodASM(Some(stmt.initialization)).code
    val terminationCode = MethodASM.methodASM(Some(stmt.termination)).code
    val incrementCode = MethodASM.methodASM(Some(stmt.increment)).code
    val bodyCode = MethodASM.methodASM(Some(stmt.body)).code

    ASM(s"""
           |;; FOR ( ${stmt.initialization} ${stmt.termination} ${stmt.increment} )
           |${initializationCode}
           |.start_for${myCounter}:
           |${bodyCode}
           |;; increment
           |${incrementCode}
           |;; check condition and jump as appropriate
           |${terminationCode}
           |;; determine if should jump
           |cmp eax, 0
           |je .end_for${myCounter}
           |jmp .start_for${myCounter}
           |.end_for${myCounter}:
           |""".stripMargin)
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
