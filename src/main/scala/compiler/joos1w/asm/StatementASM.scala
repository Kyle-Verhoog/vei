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
    val conditionCode = MethodASM.methodASM(Some(stmt.expr), lvalue = false)
    val bodyCode = MethodASM.methodASM(Some(stmt.body), lvalue = false)

    ASM(s"""
           |;; While ( ${stmt.expr} )
           |.start_while${myCounter}:""") ++
      conditionCode ++
      ASM(s"""
           |cmp eax, 0
           |je .end_while${myCounter}""") ++
      bodyCode ++
      ASM(s"""
           |jmp .start_while${myCounter}
           |.end_while${myCounter}:""")
  }

  def forStatementASM(stmt: ForStatement): ASM = {
    val myCounter = incrementAndReturnCounter

    val initializationCode =
      MethodASM.methodASM(Some(stmt.initialization), lvalue = false)
    val terminationCode =
      MethodASM.methodASM(Some(stmt.termination), lvalue = false)
    val incrementCode =
      MethodASM.methodASM(Some(stmt.increment), lvalue = false)
    val bodyCode = MethodASM.methodASM(Some(stmt.body), lvalue = false)

    ASM(
      s";; FOR ( ${stmt.initialization} ${stmt.termination} ${stmt.increment} )") ++
      initializationCode ++
      ASM(s".start_for${myCounter}:") ++
      bodyCode ++
      ASM(s";; increment") ++
      incrementCode ++
      ASM(s";; check condition and jump as appropriate") ++
      terminationCode ++
      ASM(s"") ++
      ASM(s""";; determine if should jump
             |cmp eax, 0
             |je .end_for${myCounter}
             |jmp .start_for${myCounter}
             |.end_for${myCounter}:
       """.stripMargin)
  }

  def topLevelIfStatementASM(expr: TopLevelIf): ASM = {
    val children = expr.children
    ifStatementChildrenASM(children)
  }

  def ifStatementChildrenASM(children: List[AST]): ASM = {
    if (children.isEmpty) return ASM("")

    children.head match {
      case child: IfStatement =>
        val myCounter = incrementAndReturnCounter
        val conditionCode =
          MethodASM.methodASM(Some(child.expr), lvalue = false)
        val bodyCode = MethodASM.methodASM(Some(child.body), lvalue = false)
        val elseCode = ifStatementChildrenASM(children.tail)

        ASM(s";; IF ( ${child.expr} )") ++
          conditionCode ++
          ASM(s"""
               |cmp eax, 0
               |je .else${myCounter}""") ++
          bodyCode ++
          ASM(s"""
               |jmp .endif${myCounter}
               |.else${myCounter}:""") ++
          elseCode ++
          ASM(s""".endif${myCounter}:""")
      case child: TopLevelIf =>
        topLevelIfStatementASM(child)
      case child =>
        if (children.length > 1)
          throw new RuntimeException(
            "Encountered non if statement child in the middle of an if statement list")
        MethodASM.methodASM(Some(child), lvalue = false)
    }
  }

}
