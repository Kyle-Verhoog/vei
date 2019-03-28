package compiler.joos1w.asm

import compiler.joos1w.ast._

object CommonASM {
  var strCount = 0

  def genStrLitLabel: String = {
    val i = strCount
    strCount += 1
    s"str_$i"
  }

  def commonASM(ast: Option[AST], recurseMethod: Option[AST] => ASM): ASM = {
    ast match {
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(boolAST: literals.BooleanLiteral) =>
        val intVal = if (boolAST.value) 1 else 0
        ASM(s"mov eax, $intVal ;; boolean literal")
      case Some(nullAST: literals.NullLiteral) =>
        ASM(s"mov eax, 0 ;; null literal")
      case Some(strAST: literals.StringLiteral) =>
        val str = strAST.value
        val strLabel = genStrLitLabel
        new ASM(
          text = s"mov eax, $strLabel",
          data = s"""
              | $strLabel: db $str
            """.stripMargin
        )
      case Some(strAST: literals.CharacterLiteral) =>
        ASM(s";; TODO character literal")
      case Some(vd: VariableDeclarator) =>
        commonASM(Some(vd.expression), recurseMethod)
      case Some(expr: ConditionalExpression) =>
        ExpressionASM.conditionalExpressionASM(expr, recurseMethod)
      case Some(expr: GeneralExpression) =>
        ExpressionASM.generalExpressionASM(expr, recurseMethod)
      case Some(expr: UnaryExpression) =>
        ExpressionASM.unaryExpressionASM(expr, recurseMethod)
      case Some(castExpression: CastExpression) =>
        ASM(s";; TODO cast expression")
      case Some(methodInvocation: MethodInvocation) =>
        ASM(s";; TODO method invocation")
      case Some(arrayAccess: ArrayAccess) =>
        ASM(s";; TODO array access")
      case Some(arrayCreationExpression: ArrayCreationExpression) =>
        ASM(s";; TODO array creation")
      case Some(classInstanceCreation: ClassInstanceCreation) =>
        ASM(s";; TODO class instance creation")
      case Some(name: Name) =>
        ASM(s";; name")
      case Some(_: Empty) => ASM("")
      case Some(ast: AST) =>
        throw new MatchError(s"commonASM match error on $ast ${ast.toStrTree}")
      case _ => throw new MatchError(s"commonASM match error on $ast")
    }
  }
}
