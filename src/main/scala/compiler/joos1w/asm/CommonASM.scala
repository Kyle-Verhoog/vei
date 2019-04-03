package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._
import compiler.joos1w.environment.environment.determineType
import compiler.joos1w.environment.types._

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

  def commonASM(ast: Option[AST],
                recurseMethod: (Option[AST], Boolean) => ASM,
                lvalue: Boolean): ASM = {
    ast match {
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(boolAST: literals.BooleanLiteral) =>
        if (boolAST.value) ASM("""
               |mov eax, 0xffffffff ;; eax := true
             """.stripMargin)
        else ASM("mov eax, 0 ;; eax := false")
      case Some(charAST: literals.CharacterLiteral) =>
        val charIntVal = Joos1WCodeGen.stringToChar(charAST.value).toInt
        ASM(
          s"mov eax, $charIntVal ;; eax := char literal here ${charAST.value}")
      case Some(nullAST: literals.NullLiteral) =>
        ASM(s"mov eax, 0 ;; null literal")
      case Some(strAST: literals.StringLiteral) =>
        val str = strAST.value
        val strLabel = genStrLitLabel

        val newString = str.drop(1).dropRight(1)
        var strASM = ""

        var i = 0
        var len = 0
        while (i < newString.length) {
          if (newString(i) != '\\') {
            strASM += s"dd ${newString(i).toInt}\n"
          } else {
            strASM += s"dd ${Joos1WCodeGen.stringToChar(''' + newString.substring(i, i + 2) + ''').toInt}\n"
            i += 1
          }
          i += 1
          len += 1
        }

        // TODO: these are hardcoded values for the String class
        val clsSize = 4 * (1 + 1 + 1)
        val clsLabel = "java_lang_String"
        new ASM(
          text = s"""
               |mov eax, $clsSize
               |call __malloc
               |mov ebx, $clsLabel ;; store class pointer as first item in obj
               |mov [eax], ebx
               |push eax ;; push obj ref to new string as arg
               |;; push char array literal as argument to string constructor
               |mov eax, $strLabel ;; string literal $str
               |push eax ;; push the char array argument
               |call java_lang_String_String_char__
               |add esp, 8
               |;; eax has obj pointer
             """.stripMargin,
          data = s"""
              |$strLabel:
              |dd 0                 ;; null obj reference
              |dd ${len} ;; length of char array
              |$strASM
            """.stripMargin
        )
      case Some(vd: VariableDeclarator) =>
        ASM(s";; variable declaration $vd") ++
          commonASM(Some(vd.expression), recurseMethod, lvalue)
      case Some(expr: ConditionalExpression) =>
        ExpressionASM.conditionalExpressionASM(expr, recurseMethod, lvalue)
      case Some(expr: GeneralExpression) =>
        ExpressionASM.generalExpressionASM(expr, recurseMethod, lvalue)
      case Some(expr: UnaryExpression) =>
        ExpressionASM.unaryExpressionASM(expr, recurseMethod, lvalue)
      case Some(castExpression: CastExpression) =>
        // TODO widening for numeric types
        // TODO handle casting arrays

        val myCounter = incrementAndReturnCounter
        val codeBeingCast =
          MethodASM.methodASM(Some(castExpression.beingCast), lvalue)

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
                   |;; cast: get class that it is being cast to
                   |mov eax, ${classLabel}
                   |push eax ;; save class pointer""") ++
              codeBeingCast ++
              ASM(s"""
                   |mov edx, eax ;; cast: store thing being cast to return after cast check complete
                   |mov eax, [eax] ;; eax <- class(eax)
                   |pop ebx ;; restore class pointer of type being casted to
                   |push edx
                   |;; perform cast check
                   |mov edx, [eax + 4] ;; get offset to subclass table for thing that is being cast
                   |mov ecx, [ebx + 8] ;; get offset of subclass table for type being cast to
                   |mov eax, 0xffffffff
                   |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                   |je .cast_expression_pass${myCounter}
                   |mov ebx, ${myCounter}
                   |call __exception
                   |.cast_expression_pass${myCounter}:
                   |pop eax  ;; cast: restore obj ref
                   |""".stripMargin)
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

        // whether or not the method call is an implicit this.method() call
        val isThisMethod = methodInvocation.name == methodAST.identifier

        // Get the object reference for the method call or null if it's a static
        // method
        val objRefCode = if (isStatic) {
          ASM(s"""
               |mov eax, 0 ;; null argument for static method
               |push eax
             """.stripMargin)
        } else if (isThisMethod && methodInvocation
                     .children(1)
                     .isInstanceOf[Name]) {
          val enclosingMethod = methodInvocation.env.findEnclosingMethod()
          val offset = 4 * enclosingMethod.paramCount
          ASM(s";; implicit this.${methodAST.identifier} method call") ++
            ASM(s"""
                 |mov eax, [ebp + $offset] ;; "this" should be in frame pointer
                 |push eax
               """.stripMargin)
        } else if (!methodInvocation.children(1).isInstanceOf[Name]) {
          ASM(s";; resolving method invocation prefix") ++
            commonASM(Some(methodInvocation.children(1)), recurseMethod, lvalue) ++
            ASM(s"push eax ;; push obj reference as arg")
        } else {
          val baseName =
            methodInvocation.name.split("\\.").dropRight(1).mkString(".")
          val name = new Name(baseName)
          name.env = methodInvocation.env
          ASM(s";; find base object/field for method invocation") ++
            NameASM.nameASM(Some(name), lvalue) ++
            ASM(s"push eax ;; push obj reference as arg")
        }

        val params = methodInvocation.parameters

        // Parameter pushing code
        val argPushCode =
          params
            .map(param => {
              commonASM(Some(param), recurseMethod, lvalue) ++
                ASM(s"push eax ;; push param $param")
            })
            .fold(ASM(""))(_ ++ _)

        val argPopCode = ASM(s"add esp, ${4 * (params.length + 1)} ;; pop args")

        val methodLabel = Joos1WCodeGen.methodDefinitionLabel(methodEnv)
        val methodCallCode = ASM(s"call $methodLabel")

        ASM(s""";; method invocation $methodInvocation""".stripMargin) ++
          objRefCode ++
          argPushCode ++
          methodCallCode ++
          argPopCode
      case Some(arrayAccess: ArrayAccess) =>
        val myCounter = incrementAndReturnCounter
        val arrayPointer =
          MethodASM.methodASM(
            Some(arrayAccess.primary),
            if (lvalue) false else lvalue) // prevent the lvalue from propagating
        val index = MethodASM.methodASM(
          Some(arrayAccess.expression),
          if (lvalue) false else lvalue) // prevent the lvalue from propagating
        ASM(s"""
               |;; array ${if (lvalue) "lvalue" else "rvalue"} access: ${arrayAccess.primary} size: [${arrayAccess.expression}])
               |""") ++
          arrayPointer ++
          ASM(s"""
               |;; the pointer to the array is now in eax, first we check index bounds
               |push eax ;; store array pointer""") ++
          index ++
          ASM(s"""
               |;; eax has array index
               |pop ebx ;; get array pointer
               |mov ecx, [ebx + 4] ;; ecx <- array size
               |mov edx, ebx ;; copy array pointer
               |cmp eax, ecx ;; perform index bounds check
               |jl .array_check_pass_upper_bound${myCounter}
               |mov ebx, ${myCounter}
               |call __exception
               |.array_check_pass_upper_bound${myCounter}:
               |cmp eax, 0 ;; perform index bounds check
               |jge .array_check_pass_lower_bound${myCounter}
               |mov ebx, ${myCounter}
               |call __exception
               |.array_check_pass_lower_bound${myCounter}:
               |add eax, 2 ;; add offset for array metadata
               |imul eax, 4
               |add ebx, eax,
               |;; edx has array pointer (&array[0])
               |;; ebx has &array[index]
               |;; [ebx] has array[index]""") ++ (if (lvalue) {
                                                    ASM(s"""
               | mov eax, ebx ;; eax <- &array[index]
               | mov ebx, edx ;; ebx <- &array
           """.stripMargin)
                                                  } else {
                                                    ASM(s"""
               | mov eax, [ebx] ;; eax <- array[index]
               | mov ebx, edx ;; ebx <- &array
           """.stripMargin)
                                                  })
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
        val clsSize = 4 * (1 + clsEnv.instanceFieldCount)

        val params = classInstanceCreation.parameters

        // Parameter code
        val argPushCode =
          params
            .map(param => {
              ASM(s";; parameter $param") ++
                commonASM(Some(param), recurseMethod, lvalue) ++
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
               |;; call default constructor
               |push eax
               |call default_constructor_$clsLabel
               |pop eax
               |;; pass arguments to constructor
               |push eax ;;  object pointer
               |""".stripMargin) ++
          argPushCode ++
          ASM(s"""
               |call $consLabel ;; Constructor should return obj pointer in eax
               |;; end class instance creation
               |;; eax has obj pointer
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

        val arraySize =
          MethodASM.methodASM(Some(arrayCreationExpression.expr), lvalue)

        var assembly =
          ASM(s"""
               |;; Create an array of type: ${arrayCreationExpression.primary} size: [${arrayCreationExpression.expr}])
               |""") ++
            arraySize ++
            ASM(s"""
                 |push eax ;; store actual array size
                 |cmp eax, 0
                 |jge .create_array${myCounter}
                 |mov ebx, ${myCounter}
                 |call __exception
                 |.create_array${myCounter}:
                 |add eax, 2 ;; add offset for array metadata
                 |imul eax, 4 ;; number of bytes for array
                 |call __malloc
                 |pop ebx ;; get the size
                 |mov [eax + 4], ebx ;; put array size into second array element""")

        // insert label for array type if it is not a primitive type
        if (arrayTypeLabel.isDefined) {
          assembly = assembly ++ ASM(s"""
                             |mov edx, ${arrayTypeLabel.get}
                             |mov [eax], edx ;; store array type pointer at position 0""")
        }

        assembly
      case Some(fieldAccess: FieldAccess) =>
        environment.determineType(fieldAccess.primary, fieldAccess.env) match {
          case customType: CustomType =>
            val clsEnv = customType.env
            val fieldEnv = clsEnv.containSet
              .get(fieldAccess.identifier, None)
              .get
              .asInstanceOf[VariableEnvironment]
            val fieldOffset = 4 * fieldEnv.fieldOffset
            val primaryAST =
              CommonASM.commonASM(
                Some(fieldAccess.primary),
                recurseMethod,
                if (lvalue) false else lvalue) // prevent the lvalue from propagating
            if (lvalue) {
              primaryAST ++
                ASM(s"""
                       |;; lvalue instance field access
                       |;; assume eax has address of object for field
                       |add eax, $fieldOffset ;; eax <- addr of ${fieldAccess.identifier}
                       |""".stripMargin)
            } else {
              primaryAST ++
                ASM(s"""
                       |;; rvalue instance field access
                       |;; assume eax has address of object for field
                       |add eax, $fieldOffset ;; eax <- addr of ${fieldAccess.identifier}
                       |mov eax, [eax] ;; eax <- ${fieldAccess.identifier}
                       |""".stripMargin)
            }
          case stringType: StringType =>
            val clsEnv = stringType.env
            val fieldEnv = clsEnv.containSet
              .get(fieldAccess.identifier, None)
              .get
              .asInstanceOf[VariableEnvironment]
            val fieldOffset = 4 * fieldEnv.fieldOffset
            val primaryASM =
              CommonASM.commonASM(
                Some(fieldAccess.primary),
                recurseMethod,
                if (lvalue) false else lvalue) // prevent the lvalue from propagating
            if (lvalue) {
              primaryASM ++
                ASM(s"""
                       |;; lvalue instance field access
                       |;; assume eax has address of object for field
                       |add eax, $fieldOffset ;; eax <- addr of ${fieldAccess.identifier}
                       |""".stripMargin)
            } else {
              primaryASM ++
                ASM(s"""
                       |;; rvalue instance field access
                       |;; assume eax has address of object for field
                       |add eax, $fieldOffset ;; eax <- addr of ${fieldAccess.identifier}
                       |mov eax, [eax] ;; eax <- ${fieldAccess.identifier}
                       |""".stripMargin)
            }
          case arrayType: ArrayType =>
            val primaryASM =
              CommonASM.commonASM(Some(fieldAccess.primary),
                                  recurseMethod,
                                  lvalue)
            primaryASM ++ ASM(s"""
                 |;; array field access (MUST be .length)
                 |;; assume eax has address to array
                 |mov eax, [eax + 4] ;; eax <- array length
               """.stripMargin)
          case x =>
            ASM(s"""
                 |;; TODO other field access ${fieldAccess.identifier} ${fieldAccess.primary}
               """.stripMargin)
        }
      case Some(thisCall: ThisCall) =>
        try {
          val methodEnv = thisCall.env.findEnclosingMethod()
          val offset = 4 * methodEnv.paramCount
          ASM(s"""
                 | mov eax, [ebp + $offset] ;; eax <- address of "this" obj reference
           """.stripMargin)
        } catch {
          case _: NoSuchMethodError =>
            ASM(s"""
                   |;; constructor fall-back obj reference: assume obj ref in first arg slot
                   |mov eax, [ebp + 8] ;; eax <- constructor
           """.stripMargin)
          case _: Throwable => ASM(";; TODO: indeterminate this reference")
        }
      case Some(name: Name) =>
        recurseMethod(Some(name), lvalue)
      case Some(assignment: Assignment) =>
        recurseMethod(Some(assignment), lvalue)
      case Some(_: Empty) => ASM("")
      case Some(hack: Hack) =>
        commonASM(hack.leftChild, recurseMethod, lvalue)
      case Some(ast: AST) =>
        throw new MatchError(s"commonASM match error on $ast ${ast.toStrTree}")
      case None => ASM("")
      case _    => throw new MatchError(s"commonASM match error on $ast")
    }
  }
}
