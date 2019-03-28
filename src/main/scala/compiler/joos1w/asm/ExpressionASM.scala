package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._

object ExpressionASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def generalExpressionASM(expr: GeneralExpression): ASM = {
    expr.operation match {
      case Some(op) =>
        val expr1Code = Joos1WCodeGen.astASM(Some(expr.firstExpr)).code
        val expr2Code = Joos1WCodeGen.astASM(Some(expr.secondExpr)).code
        op match {
          case "+" =>
            ASM(s"""
                 |;; ${expr.firstExpr} + ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |pop ebx
                 |add eax, ebx
                 |""".stripMargin)
          case "-" =>
            ASM(s"""
                 |;; ${expr.firstExpr} - ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |mov ebx, eax
                 |pop eax
                 |sub eax, ebx
                 |""".stripMargin)
          case "*" =>
            ASM(s"""
                 |;; ${expr.firstExpr} * ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |pop ebx
                 |imul eax, ebx
                 |""".stripMargin)
          case "/" =>
            ASM(s"""
                 |;; ${expr.firstExpr} / ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |mov ebx, eax
                 |pop eax
                 |mov edx, 0
                 |div ebx
                 |""".stripMargin)
        }
      case None => ASM("")
    }
  }

  def unaryExpressionASM(expr: UnaryExpression): ASM = {
    val exprCode = Joos1WCodeGen.astASM(Some(expr.subExpression)).code

    expr.operator match {
      case "!" =>
        ASM(s""";; UNARY ${expr.operator} ${expr.subExpression}
               |$exprCode
               |not eax
               |""".stripMargin)
      case _ => throw new RuntimeException("TODO IMPLEMENT")
    }
  }

  def conditionalExpressionASM(expr: ConditionalExpression): ASM = {
    expr.operator match {
      case op =>
        val expr1Code = Joos1WCodeGen.astASM(Some(expr.firstExpr)).code
        val expr2Code = Joos1WCodeGen.astASM(Some(expr.secondExpr)).code
        op match {
          case "!=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} != ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax ; expr 2 result in ebx
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jne end_not_equal${myCounter}
                   |mov ebx, 0
                   |end_not_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "==" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} != ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax ; expr 2 result in ebx
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 0
                   |jne end_equal_equal${myCounter}
                   |mov ebx, 1
                   |end_equal_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} >= ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jge end_greater_equal${myCounter}
                   |mov ebx, 0
                   |end_greater_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} > ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jg end_greater${myCounter}
                   |mov ebx, 0
                   |end_greater${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} <= ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jle end_less_equal${myCounter}
                   |mov ebx, 0
                   |end_less_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} < ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |$expr1Code
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jl end_less${myCounter}
                   |mov ebx, 0
                   |end_less${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "&&" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} && ${expr.secondExpr}
                   |$expr1Code
                   |cmp eax, 0
                   |je end_and${myCounter}
                   |$expr2Code
                   |end_and${myCounter}:
                   |""".stripMargin)
          case "||" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} || ${expr.secondExpr}
                   |$expr1Code
                   |cmp eax, 1
                   |je end_and${myCounter}
                   |$expr2Code
                   |end_and${myCounter}:
                   |""".stripMargin)

        }
    }
  }

  def topLevelIfStatementASM(expr: TopLevelIf): ASM = {
    val myCounter = incrementAndReturnCounter
    val children = expr.children
    ifStatementChildrenASM(children)
  }

  def ifStatementChildrenASM(children: List[AST]): ASM = {
    children.head match {
      case child: IfStatement =>
        val myCounter = incrementAndReturnCounter
        val conditionCode = Joos1WCodeGen.astASM(Some(child.expr)).code
        val bodyCode = Joos1WCodeGen.astASM(Some(child.body)).code
        val elseCode = ifStatementChildrenASM(children.tail)

        ASM(s"""
               |;; IF ( $conditionCode )
               |$conditionCode
               |cmp eax, 0
               |je else${myCounter}
               |${bodyCode}
               |jmp endif${myCounter}
               |else${myCounter}:
               |${elseCode}
               |endif${myCounter}:
               |""".stripMargin)
      case child =>
        if (children.length > 1)
          throw new RuntimeException(
            "Encountered non if statement child in the middle of an if statement list")
        Joos1WCodeGen.astASM(Some(child))
    }
  }

}
