package compiler.joos1w.asm

import compiler.joos1w.ast._

object CommonASM {
  var strCount = 0

  def genStrLitLabel: String = {
    val i = strCount
    strCount += 1
    s"str_$i"
  }

  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def commonASM(ast: Option[AST], recurseMethod: Option[AST] => ASM): ASM = {
    ast match {
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(boolAST: literals.BooleanLiteral) =>
        if (boolAST.value) ASM("""
               |mov eax, 0xffffffff ;; eax := true
             """.stripMargin)
        else ASM("mov eax, 0 ;; eax := false")
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
        val myCounter = incrementAndReturnCounter
        val arrayPointer = MethodASM.methodASM(Some(arrayAccess.primary))
        val index = MethodASM.methodASM(Some(arrayAccess.expression))
        ASM(s"""
               |;; Array access: ${arrayAccess.primary} size: [${arrayAccess.expression}])
               |extern __exception
               |extern __malloc""") ++
        arrayPointer ++
        ASM(s"""
               |;; the pointer to the array is now in ebx, first we check index bounds
               |push ebx ;; store array pointer
               |mov eax, [ebx] ;; get array size
               |push eax ;; store array size""") ++
        index ++
        ASM(s"""
               |;; eax has array index
               |mov edx, eax ;; edx now has array index
               |pop ecx ;; get array size
               |pop ebx ;; get array pointer
               |cmp edx, ecx ;; perform index bounds check
               |jl .array_check_pass_upper_bound${myCounter}
               |call __exception
               |.array_check_pass_upper_bound${myCounter}:
               |cmp edx, 0 ;; perform index bounds check
               |jge .array_check_pass_lower_bound${myCounter}
               |call __exception
               |.array_check_pass_lower_bound${myCounter}:
               |add edx, 1 ;; add offset for array metadata
               |imul edx, 4
               |add ebx, edx,
               |mov eax, [ebx]""")

      case Some(arrayCreationExpression: ArrayCreationExpression) =>
        val myCounter = incrementAndReturnCounter
        //val arrayType = MethodASM.methodASM(Some(arrayCreationExpression.primary))
        val arraySize = MethodASM.methodASM(Some(arrayCreationExpression.expr))

        ASM(s"""
               |;; Create an array of type: ${arrayCreationExpression.primary} size: [${arrayCreationExpression.expr}])
               |extern __exception
               |extern __malloc""") ++
        arraySize ++
        ASM(s"""
                 |push eax ;; store actual array size
                 |cmp eax, 0
                 |jge .create_array${myCounter}
                 |call __exception
                 |.create_array${myCounter}:
                 |add eax, 1
                 |imul eax, 4 ;; number of bytes for array
                 |call __malloc
                 |pop ebx ;; get the size
                 |mov [eax], ebx ;; put array size into first array element""")
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

