package compiler.joos1w.environment

import com.sun.jdi.BooleanType
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
        println(
          "looking for arg types of instance creation: " + ast.name + " in env ")
        val argTypes =
          ast.parameters.map(param => determineType(param, ast.env))
        val klassEnv = ast.env.lookupClass(ast.name).get
        val klass = klassEnv.ast.asInstanceOf[ClassDeclaration]

        println("looking for arg types " + argTypes)
        val consturctorExists = klass.constructors.exists(constructor => {
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
        }
        return
      }
      case ast: LocalVariableDeclaration => {
        if (ast.variableDeclarator.hasExpression) {
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
        println("DONE METHOD INVOK")
        return
      }
      //case ast: ClassDeclaration       => return
      //case ast: ConstructorDeclaration => return
      //case ast: ForStatement           => return
      //case ast: WhileStatement         => return
      //case ast: IfStatement            => return
      case _ =>
    }

    //if (ast.rightSibling.isDefined) verifyAST(ast.rightSibling.get)
    //if (ast.leftChild.isDefined) verifyAST(ast.leftChild.get)
    ast.children.foreach(ast => verifyAST(ast))
  }

  def determineType(ast: AST, env: GenericEnvironment): AbstractType = {
    ast match {
      case ast: Assignment              => determineType(ast.getLHS, ast.env)
      case ast: GeneralExpression       => determineExpressionType(ast, ast.env)
      case ast: CastExpression          => determineExpressionType(ast, ast.env)
      case ast: ConditionalExpression   => determineExpressionType(ast, ast.env)
      case ast: UnaryExpression         => determineExpressionType(ast, ast.env)
      case ast: Name                    => determineNameTtype(ast, ast.env)
      case ast: MethodInvocation        => determineMethodInvocationType(ast, ast.env)
      case ast: FieldAccess             => determineFieldAccessType(ast, ast.env)
      case ast: ArrayAccess             => determineArrayAccessType(ast, ast.env)
      case ast: BooleanLiteral          => ast.ttype
      case ast: CharacterLiteral        => ast.ttype
      case ast: IntegerLiteral          => ast.ttype
      case ast: NullLiteral             => ast.ttype
      case ast: StringLiteral           => ast.ttype
      case ast: ArrayCreationExpression => determineType(ast.primary, ast.env)
      case ast: ClassInstanceCreation => {
        println("DETERMINING CLASS INSTANCE CREATION TYPE")
        determineNameTtype(ast.primary, ast.env)
      }
      case ast: PrimitiveType => ast.ttype
      case ast: ThisCall =>
        new CustomType(ast.env.findEnclosingClass())
      case ast: LocalVariableDeclaration =>
        buildTypeFromString(ast.ttype, ast.env)
      case ast: FieldDeclaration => buildTypeFromString(ast.fieldType, ast.env)
      case ast: FormalParameter  => buildTypeFromString(ast.ttype, ast.env)
      case _ =>
        throw new RuntimeException("Unknown how to get type for ast: " + ast)
    }
  }

  def determineFieldAccessType(access: FieldAccess,
                               env: GenericEnvironment): AbstractType = {
    if (access.primary.isInstanceOf[Empty]) {
      val ttype = env
        .findEnclosingClass()
        .containSet((access.identifier, None))
        .asInstanceOf[VariableEnvironment]
        .ttype
      return types.buildTypeFromString(ttype, env)
    }

    val primaryType = determineType(access.primary, env)
    val klass = env.lookupClass(primaryType.stringType).get
    val ttype = klass
      .containSet((access.identifier, None))
      .asInstanceOf[VariableEnvironment]
      .ttype
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
        if (ast.simpleType.isDefined) {
          types.buildTypeFromString(ast.simpleType.get, ast.env)
        } else { // it has an expression as its type
          determineType(ast.children.head, env)
        }
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

        if (ttype1.isString || ttype2.isString) {
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
      case ">=" | ">" | "<=" | "<" | "!=" | "==" => {
        if (!ttype1.equals(ttype2)) {
          throw EnvironmentError(
            "Cannot compare two types that are not the same! type1: " + ttype1 + " type2: " + ttype2)
        }
        new types.BooleanType()
      }
      case "|" | "&" => {
        throw EnvironmentError("TODO: implement") //TODO
      }
      case "INSTANCEOF" => {
        throw EnvironmentError("TODO: implement") //TODO
      }
      case _ => throw new RuntimeException("Unexpected binary operation: " + op)
    }
  }

  def convertClassEnvToType(env: ClassEnvironment): AbstractType = {
    buildTypeFromString(env.qualifiedName, env)
  }

  def determineInstanceStringType(name: String,
                                  parentType: AbstractType): AbstractType = {
    val instanceFields = name.split('.')
    var callingType = parentType

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

          callingType = fieldType
        }
        case ttype: StringType => {
          val fieldEnv =
            ttype.env.containSet(field, None).asInstanceOf[VariableEnvironment]
          val fieldType = fieldEnv.abstractType

          // confirm we can access this field
          verifyUsagePermission(fieldEnv, ttype.env)

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
    println("DEtermining name type " + ast.name + " in env " + env)
    val splitName = ast.name.split('.').toList

    //val enclosingClass = env.findEnclosingClass()
    val head = splitName.head

    // look for local variable
    if (env.serarchForVariable(head).isDefined) {
      val localVar = env.serarchForVariable(head).get
      val localVarType = localVar.abstractType

      if (splitName.length == 1) return localVarType

      val instanceFields =
        splitName.slice(1, splitName.length + 1).mkString(".")
      return determineInstanceStringType(instanceFields, localVarType)
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
  def verifyUsagePermission(method: MethodEnvironment,
                               env: GenericEnvironment): Unit = {
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

  def verifyUsagePermission(field: VariableEnvironment,
                            env: GenericEnvironment): Unit = {
    val enclosing = env.findEnclosingClass()
    println("verifying usage of " + field.ast.toStrTree + " in env " + env.ast)
    val fieldEnclosing = field.findEnclosingClass()

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

    // verify array assignability
    ttype1 match {
      case ttype1: ArrayType => {
        ttype2 match {
          case ttype2: ArrayType => {
            if (ttype1.rootType == ttype2.rootType) return
            if (ttype2.rootType.isSubClassOf(ttype1.rootType)) return // sub class
          }
          case _ => verifyAssignment(ttype1.rootType, ttype2, env)
        }
        return
      }
      case _ =>
    }

    if (ttype1.equals(ttype2)) return
    if (ttype1.isInstanceOf[ShortType] && ttype2.isInstanceOf[BytesType]) return
    if (ttype1.isInstanceOf[IntType] && ttype2.isInstanceOf[CharType]) return
    if ((ttype1.isInstanceOf[CustomType] || ttype1
          .isInstanceOf[StringType]) && ttype2.isInstanceOf[NullType]) return

    println(
      "checking if " + ttype1 + " can be assigned value of " + ttype1 + " is sub class " + ttype2
        .isSubClassOf(ttype1))
    if (ttype2.isSubClassOf(ttype1)) return // sub class

    throw new RuntimeException(
      "TODO: Implement or genuinely bad assignment?" + ttype1 + " " + ttype2)
  }
}
