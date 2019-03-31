package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._
import compiler.joos1w.environment.environment.determineType
import compiler.joos1w.environment.types.{ArrayType, CustomType, StringType}

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

  //def procedureCall(objRef: ASM, params: List[AST]): ASM = {}

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
        // TODO widening for numeric types
        // TODO handle casting arrays

        val myCounter = incrementAndReturnCounter
        val codeBeingCast = MethodASM.methodASM(Some(castExpression.beingCast))

        var typeCastTo = if (castExpression.simpleType.isDefined) {
          types.buildTypeFromString(castExpression.simpleType.get,
                                    castExpression.env)
        } else { // it has an expression as its type
          determineType(castExpression.children.head, castExpression.env)
        }

        // if typeCastTo is an array, get the actual type
        if (typeCastTo.isInstanceOf[ArrayType]) {
          typeCastTo = typeCastTo.asInstanceOf[ArrayType].rootType
        }

        typeCastTo match {
          case ttype @ ((_: StringType) | (_: CustomType)) =>
            val classLabel = ttype match {
              case ttype: StringType => Joos1WCodeGen.classLabel(ttype.env)
              case ttype: CustomType => Joos1WCodeGen.classLabel(ttype.env)
            }

            ASM(s"""
                   |;;CAST EXPRESSION from ${castExpression.beingCast} to ${typeCastTo}
                   |;; get class that it is being cast to
                   |mov ebx, ${classLabel}
                   |push ebx ;; save class pointer""") ++
              codeBeingCast ++
              ASM(s"""
                   |push eax ; store thing being cast to return after cast check complete
                   |mov eax, ebx ;; eax has pointer to thing being cast
                   |mov eax, [eax] ;; need to dereference
                   |pop ebx ;; restore class pointer of type being casted to
                   |;; perform cast check
                   |mov ecx, [ebx + 8] ;; get offset of subclass table for type being cast to
                   |mov edx, [eax + 4] ;; get offset to subclass table for thing that is being cast
                   |mov eax, 0xffffffff
                   |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                   |pop eax ;; restore the thing being cast into eax
                   |je .cast_expression_pass${myCounter}
                   |call __exception
                   |.cast_expression_pass${myCounter}:""".stripMargin)
          case _ => ASM(s"""
               |;;TODO PRIMITIVE CAST CHECK
               |
             """.stripMargin) ++ codeBeingCast // TODO primitive casts
        }
      case Some(methodInvocation: MethodInvocation) =>
        val methodEnv = methodInvocation.methodDefinition match {
          case Some(methodEnvironment: MethodEnvironment) => methodEnvironment
          case None =>
            throw new MatchError(
              "kevin lied to me about method invocations having environments")
        }
        val methodAST = methodEnv.myAst.asInstanceOf[MethodDeclaration]
        val isStatic = methodAST.modifiers.contains("static")

        val containingCls = methodInvocation.env.findEnclosingClass()
        val methodInCls = containingCls.declareSet.values.toSet contains methodEnv

        // whether or not the method call is an implicit this.method() call
        val isThisMethod = methodInvocation.name == methodAST.identifier
        val offset = methodEnv.vtOffset

        // Get the object reference for the method call or null if it's a static
        // method
        val objRefCode = if (isStatic) {
          ASM(s"""
               |mov eax, 0 ;; null argument for static method
               |push eax
             """.stripMargin)
        } else if (isThisMethod) {
          ASM(s";; Implicit this.${methodAST.identifier} method call") ++
            ASM(s"""
                 |mov eax, ebp ;; "this" should be in frame pointer
                 |push eax
               """.stripMargin)
        } else {
          val baseName =
            methodInvocation.name.split("\\.").dropRight(1).mkString(".")
          val name = new Name(baseName)
          name.env = methodInvocation.env
          ASM(s";; Find base object/field for method invocation") ++
            NameASM.nameASM(Some(name)) ++
            ASM(s"push eax ;; push obj reference as arg")
        }

        val params = methodInvocation.parameters

        // Parameter pushing code
        val argPushCode =
          params
            .map(param => {
              commonASM(Some(param), recurseMethod) ++
                ASM(s"push eax ;; push param $param")
            })
            .fold(ASM(""))(_ ++ _)

        val argPopCode = ASM(s"add esp, ${4 * (params.length + 1)} ;; pop args")

        val methodLabel = Joos1WCodeGen.methodDefinitionLabel(methodEnv)
        val methodCallCode =
          if (methodInCls) {
            ASM(s"""
                 |call $methodLabel ;; invoke method ${methodAST}
             """.stripMargin)
          } else {
            ASM(s"""
                 |call $methodLabel ;; invoke external method ${methodAST}
               """.stripMargin)
          }

        ASM(s""";; Method invocation $methodInvocation""".stripMargin) ++
          objRefCode ++
          argPushCode ++
          methodCallCode ++
          argPopCode
      case Some(arrayAccess: ArrayAccess) =>
        val myCounter = incrementAndReturnCounter
        val arrayPointer = MethodASM.methodASM(Some(arrayAccess.primary))
        val index = MethodASM.methodASM(Some(arrayAccess.expression))
        ASM(s"""
               |;; Array access: ${arrayAccess.primary} size: [${arrayAccess.expression}])
               |""") ++
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
        /**
          * Instances have the following form
          *  [  addr of class    ]
          *  [  instance field 1 ]
          *  [  instance field 2 ]
          *  [       ....        ]
          *  [  instance field n ]
          */
        val env = classInstanceCreation.env
        val clsEnv = env.serarchForClass(classInstanceCreation.name).get
        val clsLabel = Joos1WCodeGen.classDefinitionLabel(clsEnv)
        // 1 for class vpointer
        val clsSize = 4 * (1 + clsEnv.numFields)

        val params = classInstanceCreation.parameters

        // Parameter code
        val argPushCode =
          params
            .map(param => {
              ASM(s";; Parameter $param") ++
                commonASM(Some(param), recurseMethod) ++
                ASM("push eax")
            })
            .fold(ASM(""))(_ ++ _)

        val argPopCode = ASM(
          s"add esp, ${4 * (params.length + 1)} ;; pop arguments off stack")

        val constructor =
          clsEnv.getConstructor(classInstanceCreation.parameters)
        val consLabel = Joos1WCodeGen.methodDefinitionLabel(constructor)

        ASM(s""";; begin class instance creation new $classInstanceCreation
               |mov eax, $clsSize
               |call __malloc
               |mov ebx, $clsLabel ;; store class pointer as first item in obj
               |mov [eax], ebx
               |mov edx, eax  ;; store object location in edx to use later TODO?
               |push edx ;; save object pointer
               |;; pass arguments to constructor
               |""".stripMargin) ++
          argPushCode ++
          ASM(s"""
               |call $consLabel ;; Constructor should return obj pointer in eax
               |;; end class instance creation
           """.stripMargin) ++
          argPopCode
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
               |""") ++
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
                             |mov edx, ${arrayTypeLabel.get}
                             |mov [eax + 4], edx ;; store array type pointer""")
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
