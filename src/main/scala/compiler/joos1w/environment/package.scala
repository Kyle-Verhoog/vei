package compiler.joos1w.environment

import compiler.joos1w.ast.literals._
import compiler.joos1w.ast.{CompilationUnit, _}
import compiler.joos1w.environment.types.numeric._
import compiler.joos1w.environment.types._
import exceptions.EnvironmentError

package object environment {
  // Identifier, Parameters
  type Signature = (String, Option[List[String]])

  def isPrimitive(ttype: String): Boolean = {
    if (List("int",
             "short",
             "boolean",
             "byte",
             "char",
             "int[]",
             "short[]",
             "boolean[]",
             "byte[]",
             "char[]",
             "void").contains(ttype))
      return true
    false
  }

  def verifyType(ttype: String, env: GenericEnvironment): Boolean = {
    var typeToVerify = ttype
    if (ttype.length > 2 && ttype.takeRight(2) == "[]") {
      typeToVerify = ttype.dropRight(2)
    }
    isPrimitive(typeToVerify) || env.lookupClass(typeToVerify).isDefined
  }

  def buildEnvironment(
      ast: AST,
      parent: Option[GenericEnvironment]): GenericEnvironment = {
    var environment: GenericEnvironment = null // TODO null?
    var parentEnvironment = parent
    // TODO finish
    ast match {
      case ast: CompilationUnit =>
        if (ast.packageDeclaration.isDefined) {
          val packageDeclaration = ast.packageDeclaration.get
          environment = parentEnvironment.get.createOrReturnRootPackageEnv(
            packageDeclaration.name)
          parentEnvironment = Option(environment)
        } else {
          // look up default package
          environment = parentEnvironment.get.createOrReturnRootPackageEnv("")
          parentEnvironment = Option(environment)
        }

        // TODO what to do with imports?
        if (ast.importDeclarations.isDefined) {}

        // special case for compilation unit, we manually recurse and return within the match
        if (ast.typeDeclaration.isDefined) {
          buildEnvironment(ast.typeDeclaration.get, parentEnvironment)
        }
        return environment
      // packages declaration
      case ast: PackageDeclaration =>
        throw new RuntimeException("should not recurse on package decl")
      // variable declaration
      case ast: LocalVariableDeclaration =>
        // we insert variables into their own environment, instead of parents
        // so we can tell if a variable is used before being declared
        environment = new VariableEnvironment(ast, parentEnvironment)
        environment.insertLocalVariable(
          ast.name,
          environment.asInstanceOf[VariableEnvironment])
      case ast: FieldDeclaration =>
        environment = new VariableEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertLocalVariable(
          ast.name,
          environment.asInstanceOf[VariableEnvironment])
      case ast: FormalParameter =>
        environment = new VariableEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertLocalVariable(
          ast.name,
          environment.asInstanceOf[VariableEnvironment])
      // class/interfaces declaration
      case ast: ClassDeclaration =>
        environment = new ClassEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertClass(
          ast.identifier,
          environment.asInstanceOf[ClassEnvironment])
      case ast: InterfaceDeclaration =>
        environment = new ClassEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertClass(
          ast.identifier,
          environment.asInstanceOf[ClassEnvironment])
      // methods declaration
      case ast: AbstractMethodDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(
          ast.signature,
          environment.asInstanceOf[MethodEnvironment])
      case ast: MethodDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(
          ast.signature,
          environment.asInstanceOf[MethodEnvironment])
        print(ast.toStrTree)
      case ast: ConstructorDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(
          ast.signature,
          environment.asInstanceOf[MethodEnvironment])
      // other blocks (for, while, etc...)
      case ast: ForStatement =>
        environment = new BlockEnvironment(ast, parentEnvironment)
      case ast: WhileStatement =>
        environment = new BlockEnvironment(ast, parentEnvironment)
      case ast: IfStatement =>
        environment = new BlockEnvironment(ast, parentEnvironment)
      case ast: ASTList => {
        // TODO is this how we do blocks?
        if (ast.getFieldName == "block_statements") {
          environment = new BlockEnvironment(ast, parentEnvironment)
        }
      }
      case _ =>
    }

    ast.env = environment
    if (environment == null && parentEnvironment.isDefined) {
      ast.env = parentEnvironment.get
    }

    if (ast.env == null) {
      println("huhhhhh???? " + ast)
    }

    if (environment != null && parentEnvironment.isDefined) {
      parentEnvironment.get.insertChild(environment)
    }

    // but make variables parent going forward, to make checking for not yet declared things easy
    ast match {
      case ast: LocalVariableDeclaration =>
        parentEnvironment = Some(environment)
      //case ast: FormalParameter => parentEnvironment = Some(environment)
      case _ =>
    }

    // recurse down
    if (ast.leftChild.isDefined) {
      if (environment != null) {
        buildEnvironment(ast.leftChild.get, Some(environment))
      } else { // if null we want to add to parent environment
        buildEnvironment(ast.leftChild.get, parentEnvironment)
      }
    }

    // recurse across
    if (ast.rightSibling.isDefined)
      buildEnvironment(ast.rightSibling.get, parentEnvironment)

    environment
  }

  /*
    Will traverse through the tree, checking to make sure
    all variables were declared, types are correct, etc...
   */
  def verifyEnvironment(env: GenericEnvironment): Unit = {
    // verify ast for most types of environments
    env match {
      case env: RootEnvironment    => // do nothing for root env AST
      case env: PackageEnvironment => // do nothing for package env AST
      //env.classTable.keys
      // TODO
      /*
           we need to traverse down only some of the children nodes (eg. the bodies for the
           declarations, but not the headers/invocations stuff like that
       */
      case env: MethodEnvironment => {
        env.ast match {
          // for methods we want to verify return type, and traverse body
          case ast: MethodDeclaration => {
            if (!verifyType(ast.returnType, env)) {
              throw EnvironmentError("Unknown return type: " + ast.returnType)
            }
            println("verifying method env body " + env.ast.toStrTree)
          }
          // just verify return type, no body to check
          case ast: AbstractMethodDeclaration => {
            if (!verifyType(ast.returnType, env)) {
              throw EnvironmentError("Unknown return type: " + ast.returnType)
            }
          }
          // just check body, no return type to verify
          case ast: ConstructorDeclaration => {
            if (env.findEnclosingClass().qualifiedName != "java.lang.Object") {
              env // verify super class has non-zero arg constructor
                .findEnclosingClass()
                .superSetClasses
                .foreach(superClass => {
                  superClass.ast match {
                    case klass: ClassDeclaration =>
                      if (!klass.hasZeroArgConstructor) {
                        throw EnvironmentError(
                          "Extending a class that does not contain a zero argument consturctor! " + klass)
                      }
                    case _ =>
                  }
                })
            }
          }
        }
      }
      // for variable declarations we want to verify types and thats it
      case env: VariableEnvironment => {
        var ttype = "TEMP"
        env.ast match {
          case ast: LocalVariableDeclaration => ttype = ast.ttype
          case ast: FieldDeclaration         => ttype = ast.fieldType
          case ast: FormalParameter          => ttype = ast.ttype
        }
        if (!verifyType(ttype, env)) {
          throw EnvironmentError("Unknown variable type: " + ttype)
        }
      }
      case _ => {}
    }

    // do checks on the environments themselves
    env match {
      case env: ClassEnvironment =>
        println("verifying class env " + env.qualifiedName)
        // verify no cycle
        env.verifyNoCyclesInExtends()

        // verify all imported packages exist
        env.verifyImportedPackagsExist()

        env.verifyNoPackageCollidesWithName()

        env.verifySingleTypeImportsExist()

        env.verifyAbstractProperties()

        env.verifyImplementsAreInterfaces()

        env.verifyDontExtendFinalClass()

        // if we are an interface, we cannot have a (Class getClass) method declared
        env.ast match {
          case ast: InterfaceDeclaration =>
            if (env.declareSet.contains("getClass", Option(List()))) {
              throw EnvironmentError(
                "Cannot declare getClass( ) in an Interface")
            }
          case _ =>
        }

        // verify there are no duplicate imported types
        val importedTypes = env.getImportSets
          .map(importSet => importSet._2)
          .filter(types => types != "*")
        if (importedTypes.length != importedTypes.distinct.length) {
          throw new RuntimeException("Duplicated imported types!")
        }

        // verify replace sets
        for (replaceSet <- env.replaceSet) {
          val m1Env = replaceSet._1
          val m2Env = replaceSet._2

          if ((m1Env.modifiers.contains("static") && !m2Env.modifiers.contains(
                "static")) || (!m1Env.modifiers.contains("static") && m2Env.modifiers
                .contains("static"))) {
            throw EnvironmentError(
              "Replacing static with non static method or vice versa. Replacing " + m1Env.signature + " with " + m2Env.signature)
          }

          if (m1Env.returnType != m2Env.returnType) {
            throw EnvironmentError(
              "Attempting to replace method with another of a different return type! Replacing " + m1Env.signature + " with " + m2Env.signature)
          }

          if (m2Env.modifiers.contains("public") && !m1Env.modifiers.contains(
                "public")) {
            throw EnvironmentError(
              "Attempting to replace public method with non public method! Replacing " + m1Env.signature + " with " + m2Env.signature)
          }

          if (m2Env.modifiers.contains("final")) {
            throw EnvironmentError(
              "Cannot replace a final method! Replacing " + m1Env.signature + " with " + m2Env.signature)
          }
        }
      case _ =>
    }
    env.childrenEnvironments.foreach(child => verifyEnvironment(child))
  }

  // traverses an AST as part of verifying an environment,
  // only go as deep as an environments scope (eg. dont go into methods, classes, etc...)
  def verifyAST(ast: AST): Unit = {
    println("verifying ast " + ast)
    ast match {
      case ast: Assignment => {
        verifyAssignment(ast, ast.env)
      }
      case ast: ArrayCreationExpression => {
        // arrray cretion expression type must be numeric
        if (!determineType(ast.expr, ast.expr.env).isNumeric) {
          throw EnvironmentError(
            "Array Creation Expressions must be a numeric type")
        }
      }
      // Name disamibuation
      case ast: Name => {
        determineType(ast, ast.env)
      }
      case ast: FieldAccess => {
        // TODO verify this is what we want
        determineType(ast, ast.env)
      }
      // check methods are defined
      case ast: ClassInstanceCreation =>
        if (ast.env.serarchForClass(ast.name).isEmpty)
          throw EnvironmentError(
            "Attempting to create instance of not found class: " + ast.name)

        // check that argument types match a constructor params
        val argTypes =
          ast.parameters.map(param => determineType(param, ast.env))
        val klassEnv = ast.env.lookupClass(ast.name).get
        val klass = klassEnv.ast.asInstanceOf[ClassDeclaration]

        if (klass.modifiers.contains("abstract")) {
          throw EnvironmentError("Cannot instantiate abstract class!")
        }

        println("class instance creation " + ast.toStrTree)
        println(
          "looking for constructor of arg types " + argTypes + " within class " + ast.env
            .findEnclosingClass()
            .ast)
        val consturctorExists = klass.constructors.exists(constructor => {
          // verify that we can access this constructor in the current env
          verifyConstructorUsagePermission(
            constructor.env.asInstanceOf[MethodEnvironment],
            ast.env)

          val constructorParamTypes = constructor.rawParameters.map(param =>
            determineType(param, klassEnv))
          println(
            "examining consturctor " + constructor.signature + " with params " + constructorParamTypes + " to " + argTypes)
          println(constructorParamTypes == argTypes)
          constructorParamTypes == argTypes
        })

        if (!consturctorExists) {
          throw EnvironmentError(
            "No constructor exists for " + ast.name + " with param types " + argTypes)
        }
        return
      //case ast: CompilationUnit    => return
      case ast: PackageDeclaration =>
        return
      case ast: ImportDeclarationsList =>
        return
      case ast: FieldDeclaration => {
        // verify initilization assignment
        if (ast.variableDeclarator.hasExpression) {
          verifyAssignment(
            determineType(ast, ast.env),
            determineType(ast.variableDeclarator.expression, ast.env),
            ast.env
          )

          verifyFieldDeclarator(ast.env.asInstanceOf[VariableEnvironment],
                                ast.variableDeclarator.expression)
        }
        return
      }
      case ast: LocalVariableDeclaration => {
        if (ast.variableDeclarator.hasExpression) {
          println("verifying local variable declarator sub expression")
          verifyAST(ast.variableDeclarator) // verify this

          println("verifying local variable assignment")
          println(ast.toStrTree)
          verifyAssignment(
            determineType(ast, ast.env),
            determineType(ast.variableDeclarator.expression, ast.env),
            ast.env
          )
        }
        return
      }
      //case ast: FormalParameter           => return
      //case ast: InterfaceDeclaration      => return
      //case ast: AbstractMethodDeclaration => return
      //case ast: MethodDeclaration         => return
      case ast: MethodInvocation => {
        println("LOOKING AT METHOD INVOK " + ast.toStrTree)
        println("env " + ast.env)
        determineType(ast, ast.env)
        if (ast.id.isDefined) {
          verifyAST(ast.primary.get)
        }
        println("DONE METHOD INVOK")
        return
      }
      //case ast: ClassDeclaration       => return
      case ast: ConstructorDeclaration => {
        if (ast.identifier != ast.env
              .findEnclosingClass()
              .qualifiedName
              .split('.')
              .last) {
          throw EnvironmentError("Constructor name must match class name!")
        }
      }
      case ast: ForStatement => {
        if (!determineType(ast.termination, ast.termination.env)
              .isInstanceOf[BooleanType]) {
          throw EnvironmentError(
            "For statement termination condition must be a boolean!")
        }
      }
      case ast: WhileStatement => {
        if (!determineType(ast.expr, ast.expr.env).isInstanceOf[BooleanType]) {
          throw EnvironmentError("While statement condition must be a boolean!")
        }
      }
      case ast: IfStatement => {
        // verify that condition evaluates to a bool
        if (!determineType(ast.expr, ast.expr.env).isInstanceOf[BooleanType]) {
          throw EnvironmentError("If statement condition must be a boolean!")
        }
      }
      case ast: Return =>
        val returnType = determineType(ast, ast.env)
        val methodtype = buildTypeFromString(
          ast.env.findEnclosingMethod().returnType,
          ast.env.findEnclosingMethod())

        if (methodtype
              .isInstanceOf[VoidType] && !ast.expr().isInstanceOf[Empty]) {
          throw EnvironmentError(
            "Cannot have any expression after return in a void method")
        }

        try {
          verifyAssignment(methodtype, returnType, ast.env)
        } catch {
          case e: Exception => {
            throw EnvironmentError(
              s"Return type should match method return value type. Return Type: $returnType     Method Type: $methodtype")
          }
        }
      case ast: ThisCall => {
        if (ast.env.findEnclosingMethod().modifiers.contains("static")) {
          throw EnvironmentError(
            "Attempting to use this in a static method: " + ast.env
              .findEnclosingMethod()
              .signature)
        }
      }
      case _ =>
    }

    //if (ast.rightSibling.isDefined) verifyAST(ast.rightSibling.get)
    //if (ast.leftChild.isDefined) verifyAST(ast.leftChild.get)
    ast.children.foreach(ast => verifyAST(ast))
  }

  def determineType(ast: AST, env: GenericEnvironment): AbstractType = {
    ast match {
      case ast: Assignment            => determineType(ast.getLHS, ast.env)
      case ast: GeneralExpression     => determineExpressionType(ast, ast.env)
      case ast: CastExpression        => determineExpressionType(ast, ast.env)
      case ast: ConditionalExpression => determineExpressionType(ast, ast.env)
      case ast: UnaryExpression       => determineExpressionType(ast, ast.env)
      case ast: Name                  => determineNameTtype(ast, ast.env)
      case ast: MethodInvocation      => determineMethodInvocationType(ast, ast.env)
      case ast: FieldAccess           => determineFieldAccessType(ast, ast.env)
      case ast: ArrayAccess           => determineArrayAccessType(ast, ast.env)
      case ast: BooleanLiteral        => ast.ttype
      case ast: CharacterLiteral      => ast.ttype
      case ast: IntegerLiteral        => ast.ttype
      case ast: NullLiteral           => ast.ttype
      case ast: StringLiteral         => ast.ttype
      case ast: ArrayCreationExpression =>
        new ArrayType(determineType(ast.primary, ast.env))
      case ast: ClassInstanceCreation => {
        println("DETERMINING CLASS INSTANCE CREATION TYPE")
        val klassName = ast.primary.name.split('.')
        if (klassName.length == 1) {
          new CustomType(env.serarchForClass(klassName.head).get)
        } else {
          determineType(ast.primary, ast.env)
        }
      }
      case ast: PrimitiveType => ast.ttype
      case ast: ThisCall =>
        new CustomType(ast.env.findEnclosingClass())
      case ast: LocalVariableDeclaration =>
        buildTypeFromString(ast.ttype, ast.env)
      case ast: FieldDeclaration => buildTypeFromString(ast.fieldType, ast.env)
      case ast: FormalParameter  => buildTypeFromString(ast.ttype, ast.env)
      case ast: Return           => determineType(ast.expr, ast.env)
      case ast: Empty            => new VoidType()
      case _ =>
        throw new RuntimeException("Unknown how to get type for ast: " + ast)
    }
  }

  def determineFieldAccessType(access: FieldAccess,
                               env: GenericEnvironment): AbstractType = {
    if (access.primary.isInstanceOf[Empty]) {
      val field = env
        .findEnclosingClass()
        .containSet((access.identifier, None))
        .asInstanceOf[VariableEnvironment]
      val ttype = field.ttype

      verifyUsagePermission(field, env) // verify we can access this field here
      return types.buildTypeFromString(ttype, env)
    }

    val primaryType = determineType(access.primary, env)

    primaryType match {
      case primaryType: ArrayType =>
        if (access.identifier == "length") {
          return new IntType
        } else {
          throw EnvironmentError(
            "Attempting to invoke non .length field access on an array")
        }
      case primaryType: CustomType =>
      case primaryType: StringType =>
      case _ =>
        throw EnvironmentError("Attempting to invoke field on primitive type")
    }

    val klass = env.lookupClass(primaryType.stringType).get
    val field = klass
      .containSet((access.identifier, None))
      .asInstanceOf[VariableEnvironment]
    val ttype = field.ttype

    verifyUsagePermission(field, env) // verify we can access this field here
    types.buildTypeFromString(ttype, env)
  }

  def determineArrayAccessType(access: ArrayAccess,
                               env: GenericEnvironment): AbstractType = {
    if (!determineType(access.expression, env).isNumeric) {
      throw EnvironmentError(
        "Array Access indexing expression must have numeric type")
    }
    println("array access primary " + access.primary.toStrTree)
    val primaryType = determineType(access.primary, env)
    primaryType match {
      case primaryType: ArrayType => primaryType.rootType
      case _ =>
        throw EnvironmentError("array access must be performed on array type!")
    }
  }

  def determineListTypes(list: ASTList,
                         env: GenericEnvironment): List[AbstractType] = {
    list.children.map(ele => determineType(list, env))
  }

  def determineMethodInvocationType(ast: MethodInvocation,
                                    env: GenericEnvironment): AbstractType = {
    val paramsSig =
      ast.parameters.map(param => determineType(param, env).stringType)

    if (ast.id.isDefined) { // need to find out primary first
      val sig: Signature = (ast.id.get, Some(paramsSig))

      val klass = determineType(ast.primary.get, env) match {
        case primaryType: StringType => primaryType.env
        case primaryType: CustomType => primaryType.env
        case child =>
          throw EnvironmentError(
            "Attempting to invoke function on type " + child + "gotten from: \n" + ast.primary.get.toStrTree)
      }

      val method = klass.findMethodWithSignature(sig)

      if (method.isEmpty) {
        throw EnvironmentError(
          "Unable to find method for invocation with signature " + sig)
      }

      verifyUsagePermission(method.get, klass)
      types.buildTypeFromString(method.get.returnType, method.get)
    } else { // just look up the name
      println("looking up method " + ast.name)
      val methodName = ast.name.split('.')
      val sig: Signature = (methodName.last, Some(paramsSig))

      val klass = methodName.length match {
        case 1 => {
          println("finding enclosing class " + env.findEnclosingClass().ast)
          env.findEnclosingClass()
        }
        case _ => {
          println("about to find primary name with env " + env)
          val dummyName = new Name(methodName.dropRight(1).mkString("."))
          dummyName.env = env

          determineType(dummyName, env) match {
            case primaryType: StringType => primaryType.env
            case primaryType: CustomType => primaryType.env
            case child =>
              throw EnvironmentError(
                "Attempting to invoke function on type " + child)
          }
        }
      }

      val method = klass.findMethodWithSignature(sig)

      if (method.isEmpty) {
        throw EnvironmentError(
          "Unable to find method for invocation with signature " + sig)
      }

      if (methodName.length == 1) {
        verifyUsagePermission(method.get, env)
      } else {
        verifyUsagePermission(method.get, klass)
      }

      types.buildTypeFromString(method.get.returnType, method.get)
    }
  }

  def determineExpressionType(ast: AST,
                              env: GenericEnvironment): AbstractType = {
    ast match {
      case ast: GeneralExpression => {
        determineBinaryOperationType(ast.operation.get,
                                     determineType(ast.firstExpr, env),
                                     determineType(ast.secondExpr, env))
      }
      case ast: CastExpression => {
        val typeCastTo = if (ast.simpleType.isDefined) {
          types.buildTypeFromString(ast.simpleType.get, ast.env)
        } else { // it has an expression as its type
          determineType(ast.children.head, env)
        }

        val typeBeingCast = determineType(ast.beingCast, ast.env)
        println(
          "verifying cast Cannot cast type: " + typeBeingCast + " to type " + typeCastTo)

        if (typeCastTo.isNumeric && !typeCastTo
              .isInstanceOf[ArrayType] && typeBeingCast.isNumeric && !typeBeingCast
              .isInstanceOf[ArrayType]) {
          return typeCastTo
        }

        try {
          verifyAssignment(typeCastTo, typeBeingCast, ast.env)
        } catch {
          case e: Exception => {
            try {
              verifyAssignment(typeBeingCast, typeCastTo, ast.env)
            } catch {
              case e: Exception =>
                throw EnvironmentError(
                  "Cannot cast type: " + typeBeingCast + " to type " + typeCastTo)
            }
          }
        }

        typeCastTo
      }
      case ast: ConditionalExpression => {
        determineBinaryOperationType(ast.operator,
                                     determineType(ast.firstExpr, env),
                                     determineType(ast.secondExpr, env))
      }
      case ast: UnaryExpression => {
        determineUnaryOperationType(ast.operator,
                                    determineType(ast.subExpression, env))
      }
      case _ => throw new RuntimeException("Not an expression type! " + ast)
    }
  }

  def determineUnaryOperationType(op: String,
                                  ttype: AbstractType): AbstractType = {
    op match {
      case "!" =>
        ttype match {
          case ttype: types.BooleanType => new types.BooleanType()
          case _ =>
            throw EnvironmentError(
              "\"!\" operator expects boolean type not " + ttype)
        }
      case "~" =>
        throw EnvironmentError("TODO: implement") //TODO
      case "-" => {
        ttype match {
          case ttype: NumericType => new IntType()
        }
      }
      case _ => throw new RuntimeException("Unexpected unary operation: " + op)
    }
  }

  def determineBinaryOperationType(op: String,
                                   ttype1: AbstractType,
                                   ttype2: AbstractType): AbstractType = {
    op match {
      case "+" => {
        if (ttype1.isNumeric && ttype2.isNumeric) {
          return new IntType()
        }

        if (ttype1.isVoid || ttype2.isVoid) {
          throw EnvironmentError(
            "Cannot add void with non void type type1: " + ttype1 + " type2: " + ttype2)
        }

        if (ttype1.isString) {
          return new StringType(ttype1.asInstanceOf[StringType].env)
        }

        if (ttype2.isString) {
          return new StringType(ttype2.asInstanceOf[StringType].env)
        }

        throw EnvironmentError(
          "Cannot add given types type1: " + ttype1 + " type2: " + ttype2)
      }
      case "-" | "*" | "/" | "%" => {
        if (ttype1.isNumeric && ttype2.isNumeric) {
          return new IntType()
        }

        throw EnvironmentError(
          "Cannot operate on non-numeric types type1: " + ttype1 + " type2: " + ttype2)
      }
      case "!=" | "==" => {
        if (!(ttype1.equals(ttype2) || (ttype1.isSubClassOf(ttype2) || ttype2
              .isSubClassOf(ttype1)) || ((ttype1
              .isInstanceOf[CustomType] || ttype1
              .isInstanceOf[StringType]) && ttype2.isInstanceOf[NullType]))) {
          throw EnvironmentError(
            "Cannot compare two types that are not the same! type1: " + ttype1 + " type2: " + ttype2)
        }

        if (ttype1.isInstanceOf[VoidType] || ttype2.isInstanceOf[VoidType]) {
          throw EnvironmentError("Cannot have void type in equality check!")
        }

        new types.BooleanType()
      }
      case ">=" | ">" | "<=" | "<" | "&&" | "||" => {
        if (!ttype1.equals(ttype2)) {
          throw EnvironmentError(
            "Cannot compare two types that are not the same! type1: " + ttype1 + " type2: " + ttype2)
        }
        new types.BooleanType()
      }
      case "|" | "&" => {
        throw EnvironmentError("TODO: implement") //TODO
      }
      case "instanceof" => {
        if (!(ttype1.isString || ttype1.isInstanceOf[CustomType] || ttype1
              .isInstanceOf[ArrayType] || ttype1
              .isInstanceOf[NullType])) {
          throw EnvironmentError(
            "Cannot call instanceof on simple type " + ttype1)
        }

        if (!(ttype2.isString || ttype2.isInstanceOf[CustomType] || ttype2
              .isInstanceOf[ArrayType])) {
          throw EnvironmentError(
            "Cannot check that osmething is a simple type: " + ttype2)
        }

        new BooleanType
      }
      case _ => throw new RuntimeException("Unexpected binary operation: " + op)
    }
  }

  def convertClassEnvToType(env: ClassEnvironment): AbstractType = {
    buildTypeFromString(env.qualifiedName, env)
  }

  // takes an optional calling env to check that this instance
  // is accessible from that env
  def determineInstanceStringType(
      name: String,
      parentType: AbstractType,
      callingEnv: Option[GenericEnvironment] = None): AbstractType = {
    val instanceFields = name.split('.')
    var callingType = parentType
    println(
      "examining instance string with parenttype: " + parentType.stringType)
    if (callingEnv.isDefined) {
      println(" with calling env " + callingEnv.get.ast.toStrTree)
      println(callingEnv.get.findEnclosingClass().qualifiedName)
    }

    instanceFields.foreach(field => {
      println("looking at field " + field)
      callingType match {
        case ttype: ArrayType => {
          if (field == "length") {
            return new IntType() // TODO verify
          }
          callingType = ttype.rootType
        }
        case _ =>
      }

      callingType match {
        case ttype: CustomType => {
          val fieldEnv =
            ttype.env.containSet(field, None).asInstanceOf[VariableEnvironment]
          val fieldType = fieldEnv.abstractType

          // confirm we can access this field
          verifyUsagePermission(fieldEnv, ttype.env)
          // verify we can call it from calling env
          if (callingEnv.isDefined) {
            verifyCanCallFrom(callingEnv.get, ttype.env)
          }

          callingType = fieldType
        }
        case ttype: StringType => {
          val fieldEnv =
            ttype.env.containSet(field, None).asInstanceOf[VariableEnvironment]
          val fieldType = fieldEnv.abstractType

          // confirm we can access this field
          verifyUsagePermission(fieldEnv, ttype.env)
          // verify we can call it from calling env
          if (callingEnv.isDefined) {
            verifyCanCallFrom(callingEnv.get, ttype.env)
          }

          callingType = fieldType
        }
        case _ =>
          throw EnvironmentError(
            "Looking up instance fields on primitive type!")
      }
    })

    callingType
  }

  def determineNameTtype(ast: Name, env: GenericEnvironment): AbstractType = {
    println(
      "DEtermining name type " + ast.name + " in env " + env.ast.toStrTree)
    val splitName = ast.name.split('.').toList

    //val enclosingClass = env.findEnclosingClass()
    val head = splitName.head

    // look for local variable
    if (env.serarchForVariable(head).isDefined) {
      val localVar = env.serarchForVariable(head).get
      val localVarType = localVar.abstractType

      // check if trying to access non-static variable from static context
      if (!localVar.modifiers.contains("static") && env.isInMethod()) {
        val method = env.findEnclosingMethod()
        if (method.modifiers.contains("static")) {
          val classVar = method.findEnclosingClass().serarchForVariable(head)

          if (classVar.isDefined && classVar.get == localVar) {
            throw EnvironmentError(
              "Attempting to access non-static class var from static context. Method: " + method.signature + " var: " + localVar.ast.toStrTree)
          }
        }
      }

      if (splitName.length == 1) return localVarType

      val instanceFields =
        splitName.slice(1, splitName.length + 1).mkString(".")
      return determineInstanceStringType(instanceFields,
                                         localVarType,
                                         Some(ast.env))
    } else if (env.lookupClass(head).isDefined) { // found object
      val obj = env.lookupClass(head).get
      if (splitName.length == 1) return convertClassEnvToType(obj)

      val instanceFields =
        splitName.slice(1, splitName.length + 1).mkString(".")
      return determineInstanceStringType(instanceFields,
                                         convertClassEnvToType(obj))
    } else { // keep searching until we find a qualified object
      var i = 1
      while (i <= splitName.length) {
        val possibleObj = splitName.slice(0, i).mkString(".")
        if (env.lookupClass(possibleObj).isDefined) {
          val objType = env.lookupClass(possibleObj).get

          // check if there is more, if so next thing is static field
          if (i < splitName.length) {
            val staticField = splitName(i)
            if (objType.containSet.get(staticField, None).isDefined) {
              val staticEnv = objType.containSet(staticField, None)
              val staticTypeField = staticEnv.asInstanceOf[VariableEnvironment]

              // confirm we can access this field
              verifyUsagePermission(staticTypeField, env)

              if (!staticTypeField.modifiers.contains("static")) {
                throw EnvironmentError(
                  "Attempting to access non static field in a static context!")
              }

              if (i + 1 < splitName.length) {
                val instanceFields =
                  splitName.slice(i + 1, splitName.length + 1).mkString(".")
                return determineInstanceStringType(instanceFields,
                                                   staticTypeField.abstractType)
              } else { // no instance fields so we are done
                return staticTypeField.abstractType
              }
            }
          } else { // no static so we are done
            return convertClassEnvToType(objType)
          }
        }
        i += 1
      }
    }

    throw EnvironmentError(s"Could not resolve name: $splitName")
  }

  /*
  def determineNameType(ast: Name, env: GenericEnvironment): AbstractType = {
    val splitName = ast.name.split('.')
    println("looking at name  " + splitName.head)
    println("parent " + ast.parent)
    println("env " + env)
    val enclosingClass = env.findEnclosingClass()
    if (env
          .serarchForVariable(splitName.head)
          .isDefined) {
      ast.objectPart = Some(splitName.head)
      ast.objectType = Some(
        types.buildTypeFromString(
          env.serarchForVariable(splitName.head).get.ttype))

      if (splitName.length > 1) {
        ast.instanceField = Some(splitName.drop(1).mkString("."))
      }
      println("found var, instance is " + ast.instanceField)
    } else if (enclosingClass.containSet
                 .contains((splitName.head, None))) {
      ast.objectPart = Some(splitName.head)
      ast.objectType = Some(
        types.buildTypeFromString(
          enclosingClass
            .containSet(splitName.head, None)
            .asInstanceOf[VariableEnvironment]
            .ttype))

      if (splitName.length > 1) {
        ast.instanceField = Some(splitName.drop(1).mkString("."))
      }
    } else {
      var i = 1
      while (i <= splitName.length) {
        if (env
              .serarchForClass(splitName.slice(0, i).mkString("."))
              .isDefined) {
          ast.objectPart = Some(splitName.slice(0, i).mkString("."))
          ast.objectType = Some(types.buildTypeFromString(ast.objectPart.get))

          if (i < splitName.length) { // do static if still stuff left
            ast.staticField = Some(splitName(i))
            println("looking at STATIC FIELD")
            println(ast.staticField)
            println(ast.objectPart)
            println(ast.objectType)
            println(
              env
                .serarchForClass(ast.objectPart.get)
                .get
                .containSet
                .keys)
            val staticField = env
              .serarchForClass(ast.objectPart.get)
              .get
              .containSet(splitName(i), None)
              .asInstanceOf[VariableEnvironment]

            if (!staticField.modifiers.contains("static")) {
              throw EnvironmentError(
                "Accessing non-static field as a static field!")
            }

            ast.staticType = Some(
              types.buildTypeFromString(
                env
                  .serarchForClass(ast.objectPart.get)
                  .get
                  .containSet(splitName(i), None)
                  .asInstanceOf[VariableEnvironment]
                  .ttype))

            if (i + 1 < splitName.length) { // do instance fields if still stuff left
              ast.instanceField = Some(
                splitName.slice(i + 1, splitName.length).mkString("."))
            }
          }
          i = splitName.length + 1 // stop looping
        }
        i += 1
      }
    }

    // TODO verify that parts of name are identified and verify they actually exist
    if (ast.staticType.isDefined) {
      val instanceClass = env
        .serarchForClass(ast.staticType.get.stringType)
      if (instanceClass.isDefined) {
        val instance =
          instanceClass.get
            .resolveInstanceNames(ast.instanceField.get, enclosingClass)
        ast.instanceType = Some(types.buildTypeFromString(instance.ttype))
      }
    } else if (ast.objectType.isDefined && ast.instanceField.isDefined) { // do it for non static fields
      if (ast.objectType.get
            .isInstanceOf[ArrayType] && ast.instanceField.get == "length") { // special case for array length
        ast.instanceType = Some(new IntType())
      } else {
        val objectClass = env
          .serarchForClass(ast.objectType.get.stringType)
          .get

        println("about to resolv")
        println("object " + ast.objectPart + " type " + ast.objectType)
        println("object " + ast.staticField + " type " + ast.staticType)
        val instance =
          objectClass.resolveInstanceNames(ast.instanceField.get, objectClass)
        ast.instanceType = Some(types.buildTypeFromString(instance.ttype))
      }
    }

    // --- verify permissions ---
    if (ast.staticType.isDefined) {
      val klass = env.serarchForClass(ast.objectType.get.stringType).get // get class of object
      val staticField = klass
        .containSet(ast.staticField.get, None)
        .asInstanceOf[VariableEnvironment]
      verifyUsagePermission(staticField, env)
    }

    // --- return actual type ---
    if (ast.instanceType.isDefined) {
      ast.instanceType.get
    } else if (ast.staticType.isDefined) {
      ast.staticType.get
    } else {
      println(ast.name)
      println(
        env
          .serarchForVariable(splitName.head)
          .isDefined)
      println(ast.objectPart)
      println(ast.objectType)
      ast.objectType.get
    }
  }
   */

  def verifyConstructorUsagePermission(method: MethodEnvironment,
                                       env: GenericEnvironment): Unit = {
    val enclosing = env.findEnclosingClass()
    val methodEnclosing = method.findEnclosingClass()

    if (method.modifiers.contains("public")) {}
    if (method.modifiers.contains("protected") && enclosing != methodEnclosing) {
      throw EnvironmentError(
        "Attempting to use protected constructor when not in the class!")
    }
    if (method.modifiers.contains("private") && enclosing != methodEnclosing) {
      throw EnvironmentError("Attempting to use private method!")
    }
  }

  def verifyUsagePermission(method: MethodEnvironment,
                            env: GenericEnvironment): Unit = {
    // check if we are invoking non-static method in a static context
    if (env.isInMethod() && env
          .findEnclosingMethod()
          .modifiers
          .contains("static")) {
      if (!method.modifiers.contains("static")) {
        throw EnvironmentError(
          "Attempting to invoke non-static method" + method.signature + "in static context " + env
            .findEnclosingMethod()
            .signature)
      }
    }

    val enclosing = env.findEnclosingClass()
    val methodEnclosing = method.findEnclosingClass()

    if (method.modifiers.contains("public")) {}
    if (method.modifiers.contains("protected") && !enclosing.isSubClassOf(
          methodEnclosing)) {
      throw EnvironmentError(
        "Attempting to use protected method in non-sub class!")
    }
    if (method.modifiers.contains("private") && enclosing != methodEnclosing) {
      throw EnvironmentError("Attempting to use private method!")
    }
  }

  def verifyCanCallFrom(callingEnv: GenericEnvironment,
                        calledEnv: GenericEnvironment): Unit = {
    val callingClass = callingEnv.findEnclosingClass()
    val calledClass = calledEnv.findEnclosingClass()

    println(
      "Check if can call something in " + calledClass.qualifiedName + " from " + callingClass.qualifiedName)
    if (!callingClass.isSubClassOf(calledClass)) {
      throw EnvironmentError(
        "Attempting to call something in " + calledClass.qualifiedName + " from " + callingClass.qualifiedName + ", but they arent subclasses of eachother")
    }
  }

  def verifyUsagePermission(field: VariableEnvironment,
                            env: GenericEnvironment): Unit = {
    val enclosing = env.findEnclosingClass()
    println("verifying usage of " + field.ast.toStrTree + " in env " + env.ast)
    val fieldEnclosing = field.findEnclosingClass()
    println(
      "field enclsoing " + fieldEnclosing.qualifiedName + " called enclsoing " + enclosing.qualifiedName)

    if (field.modifiers.contains("public")) { return }
    if (field.modifiers.contains("protected") && !enclosing.isSubClassOf(
          fieldEnclosing)) {
      throw EnvironmentError(
        "Attempting to use protected field in non-sub class!")
    }
    if (field.modifiers.contains("private") && enclosing != fieldEnclosing) {
      throw EnvironmentError("Attempting to use private field!")
    }
  }

  def verifyAssignment(assignment: Assignment,
                       env: GenericEnvironment): Unit = {
    println("doing normal assignment")
    //println(env.ast.toStrTree)
    //println(assignment.env.ast.toStrTree)
    verifyAssignment(determineType(assignment.getLHS, env),
                     determineType(assignment.getRHS, env),
                     env)
    println("done")
  }

  def verifyAssignment(ttype1: AbstractType,
                       ttype2: AbstractType,
                       env: GenericEnvironment): Unit = {
    println("comparing " + ttype1 + " with " + ttype2 + " in env " + env)

    // check for special java built ins
    ttype1 match {
      case ttype1: CustomType => {
        ttype1.env.qualifiedName match {
          case "java.lang.Object" | "java.lang.Cloneable" |
              "java.io.Serializable" => {
            if (ttype2.isInstanceOf[ArrayType]) return
          }
          case _ =>
        }
      }
      case _ =>
    }

    // verify array assignability
    ttype1 match {
      case ttype1: ArrayType => {
        ttype2 match {
          case ttype2: ArrayType => {
            println(s"comparing two array type $ttype1 $ttype2")
            if (ttype1.rootType == ttype2.rootType) return
            if (ttype2.rootType.isSubClassOf(ttype1.rootType)) return // sub class
            throw EnvironmentError(s"Cant assign $ttype2 to $ttype1")
          }
          case ttype2: NullType => return
          case _ =>
            throw EnvironmentError("Cant assign non array type to an array!")
        }
        return
      }
      case _ =>
    }

    if (ttype1.equals(ttype2)) return
    if (ttype1.isInstanceOf[IntType] && ttype2.isInstanceOf[CharType]) return
    if (ttype1.isInstanceOf[IntType] && ttype2.isInstanceOf[ShortType]) return
    if (ttype1.isInstanceOf[ShortType] && ttype2.isInstanceOf[BytesType]) return
    if (ttype1.isInstanceOf[IntType] && ttype2.isInstanceOf[BytesType]) return // by transitivity
    if ((ttype1.isInstanceOf[CustomType] || ttype1
          .isInstanceOf[StringType]) && ttype2.isInstanceOf[NullType]) return

    println(
      "checking if " + ttype1 + " can be assigned value of " + ttype1 + " is sub class " + ttype2
        .isSubClassOf(ttype1))
    if (ttype2.isSubClassOf(ttype1)) return // sub class

    throw new RuntimeException(
      "TODO: Implement or genuinely bad assignment?" + ttype1 + " " + ttype2)
  }

  def verifyFieldDeclarator(declarationEnv: VariableEnvironment,
                            ast: AST): Unit = {
    ast match {
      case ast: Name => {
        var otherField =
          declarationEnv.findEnclosingClass().findLocalVariable(ast.name)

        if (otherField.isEmpty) {
          otherField = declarationEnv
            .findEnclosingClass()
            .findLocalVariable(ast.partsOfName.head)
        }

        // two sets of rules depending on if declaration is static or not
        if (declarationEnv.modifiers.contains("static")) {
          println("checking name " + ast)
          if (ast.name == declarationEnv.name) {
            throw EnvironmentError(
              "Illegal forward reference of self on static field initilization: " + declarationEnv.ast)
          }

          println("checking forward reference of " + declarationEnv.ast)
          println("other: " + ast.name)
          if (otherField.isDefined) {
            println("order of decl " + declarationEnv.order)
            println("order of other " + otherField.get.order)
          }

          if (otherField.isDefined && !otherField.get.modifiers.contains(
                "static")) {
            throw EnvironmentError(
              "Static field declaration cannot refer to non static fields.")
          }

          if (otherField.isDefined && otherField.get.order > declarationEnv.order) {
            throw EnvironmentError(
              "Attempting to perform forward access in static declaration of " + declarationEnv.ast + " to access " + otherField.get.ast)
          }
        } else { // non static case
          println("checking name " + ast)
          if (ast.name == declarationEnv.name) {
            throw EnvironmentError(
              "Illegal forward reference of self on static field initilization: " + declarationEnv.ast)
          }

          println("checking forward reference of " + declarationEnv.ast)
          println("other: " + ast.name)
          if (otherField.isDefined) {
            println("order of decl " + declarationEnv.order)
            println("order of other " + otherField.get.order)
          }

          if (otherField.isDefined && !otherField.get.modifiers.contains(
                "static") && otherField.get.order > declarationEnv.order) {
            throw EnvironmentError(
              "Attempting to perform forward access in declaration of " + declarationEnv.ast + " to access " + otherField.get.ast)
          }
        }
        return
      }
      case ast: Assignment => {
        // for assignments we only care about RHS when the LHS is a simple name
        if (ast.getLHS.isInstanceOf[Name] && ast.getLHS
              .asInstanceOf[Name]
              .isSimpleName) {
          verifyFieldDeclarator(declarationEnv, ast.getRHS)
          return
        }
      }
      case ast: ThisCall => {
        if (declarationEnv.modifiers.contains("static")) {
          throw EnvironmentError(
            "keyword THIS cannot occur within the declaration of a static field")
        }

        return
      }
      case ast: ClassInstanceCreation => return
      case _                          =>
    }

    ast.children.foreach(child => verifyFieldDeclarator(declarationEnv, child))
  }
}
