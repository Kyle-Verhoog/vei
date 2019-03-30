package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._
import compiler.joos1w.environment.types.{CustomType, StringType}

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
        ASM(s";; variable declaration $vd") ++
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
        val methodEnv = methodInvocation.methodDefinition match {
          case Some(methodEnvironment: MethodEnvironment) => methodEnvironment
          case None =>
            throw new MatchError(
              "kevin lied to me about method invocations having environments")
        }
        val methodAST = methodEnv.myAst.asInstanceOf[MethodDeclaration]
        val isStatic = methodAST.modifiers.contains("static")

        // whether or not the method call is an implicit this.method() call
        val isThisMethod = methodInvocation.name == methodAST.identifier
        val offset = methodEnv.vtOffset

        // Parameter code
        val argCode =
          methodInvocation.parameters
            .map(param => {
              ASM(s";; Parameter $param") ++
                commonASM(Some(param), recurseMethod) ++
                ASM("push eax")
            })
            .fold(ASM(""))(_ ++ _)

        // Get the object reference for the method call or null if it's a static
        // method
        val objRefCode = if (isStatic) {
          ASM(s"mov eax, 0 ;; null argument for static method")
        } else if (isThisMethod) {
          ASM(s";; Implicit this.${methodAST.identifier} method call") ++
            MethodASM.methodASM(Some(new ThisCall()))
        } else {
          val baseName =
            new Name(
              methodInvocation.name.split("\\.").dropRight(1).mkString("."))
          // ASM(s";; Method call on object ${baseName}") ++
          //   MethodASM.methodASM(Some(baseName))
          ASM(";; TODO method call on qualified name")
        }

        val methodCallCode =
          if (methodInvocation.name == methodAST.identifier) {
            ASM(s""";; Method invocation in same class
                 |;; TODO eval and push args
                 |
             """.stripMargin)
          } else {
            ASM(";; TODO qualified method invocation")
          }

        ASM(s""";; Method invocation $methodInvocation
             |;; Pushing args
           """.stripMargin) ++
          argCode ++
          objRefCode ++
          ASM(s"""
               | push eax
           """.stripMargin) ++
          methodCallCode
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
               |mov ebx, [ebx]
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
               |add edx, 2 ;; add offset for array metadata
               |imul edx, 4
               |add ebx, edx,
               |mov eax, [ebx]""")
      case Some(classInstanceCreation: ClassInstanceCreation) =>
        val env = classInstanceCreation.env
        val clsEnv = env.serarchForClass(classInstanceCreation.name).get
        val clsAST = clsEnv.myAst.asInstanceOf[ClassDeclaration]
        val clsLabel = Joos1WCodeGen.classDefinitionLabel(clsEnv)
        // 1 for class vpointer
        val clsSize = 4 * (1 + clsEnv.numFields)

        // Parameter code
        val argCode =
          classInstanceCreation.parameters
            .map(param => {
              ASM(s";; Parameter $param") ++
                commonASM(Some(param), recurseMethod) ++
                ASM("push eax")
            })
            .fold(ASM(""))(_ ++ _)

        val constructor =
          clsEnv.getConstructor(classInstanceCreation.parameters)
        val consLabel = Joos1WCodeGen.methodDefinitionLabel(constructor)

        val isSameClass = env.findEnclosingClass() == clsEnv
        val externClsLabel = if (!isSameClass) s"extern $clsLabel" else ""
        val externConslabel = if (!isSameClass) s"extern $consLabel" else ""
        ASM(s""";; begin class instance creation new $classInstanceCreation
               |mov eax, $clsSize
               |call __malloc
               |$externClsLabel
               |mov ebx, $clsLabel ;; store class pointer as first item in obj
               |mov [eax], ebx
               |mov edx, eax  ;; store object location in edx to use later TODO?
               |;; pass arguments to constructor""".stripMargin) ++
          argCode ++
          ASM(s"""
               |push edx ;; save object pointer
               |$externConslabel
               |call $consLabel
               |pop eax  ;; restore object pointer
               |;; end class instance creation
           """.stripMargin)
      case Some(arrayCreationExpression: ArrayCreationExpression) =>
        val myCounter = incrementAndReturnCounter

        // find the type of the array, if not a primitive then we get the label for its entry
        val ttype = environment.determineType(arrayCreationExpression.primary,
                                              arrayCreationExpression.env)
        val arrayTypeLabel: Option[String] = ttype match {
          case ttype: CustomType => Option(Joos1WCodeGen.classLabel(ttype.env))
          case ttype: StringType => Option(Joos1WCodeGen.classLabel(ttype.env))
          case _                 => None
        }

        val arraySize = MethodASM.methodASM(Some(arrayCreationExpression.expr))

        var assembly =
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
                 |add eax, 2 ;; add offset for array metadata
                 |imul eax, 4 ;; number of bytes for array
                 |call __malloc
                 |pop ebx ;; get the size
                 |mov [eax], ebx ;; put array size into first array element""")

        // insert label for array type if it is not a primitive type
        if (arrayTypeLabel.isDefined) {
          assembly = assembly ++ ASM(s"""
                             |mov [eax + 4], ${arrayTypeLabel} ;; store array type pointer""")
        }

        assembly
      case Some(fieldAccess: FieldAccess) =>
        ASM(s";; TODO field access")
      case Some(name: Name) =>
        recurseMethod(Some(name))
      case Some(_: Empty) => ASM("")
      case Some(ast: AST) =>
        throw new MatchError(s"commonASM match error on $ast ${ast.toStrTree}")
      case None => ASM("")
      case _    => throw new MatchError(s"commonASM match error on $ast")
    }
  }
}
