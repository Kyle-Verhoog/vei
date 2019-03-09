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
    isPrimitive(typeToVerify) || env.serarchForClass(typeToVerify).isDefined
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
        print(ast.toStrTree)
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
      case ast: ASTList => {
        // TODO is this how we do blocks?
        if (ast.getFieldName == "block_statements") {
          environment = new BlockEnvironment(ast, parentEnvironment)
        }
      }
      case _ =>
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
            verifyAST(env, ast.body)
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
            verifyAST(env, ast.body)
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
      case _ => verifyAST(env, env.ast)
    }

    // do checks on the environments themselves
    env match {
      case env: ClassEnvironment =>
        println("verifying class env " + env.qualifiedName)
        // verify no cycle
        env.verifyNoCyclesInExtends()
        /* println("INHERITS----")
        env.inheritSet
          .filter(ele => ele._2.isInstanceOf[MethodEnvironment])
          .foreach(
            ele =>
              println(
                "sig " + ele._1 + " mods " + ele._2
                  .asInstanceOf[MethodEnvironment]
                  .modifiers + " returns " + ele._2
                  .asInstanceOf[MethodEnvironment]
                  .returnType))
        println("CONTAINS     ----")
        env.containSet
          .filter(ele => ele._2.isInstanceOf[MethodEnvironment])
          .foreach(
            ele =>
              println(
                "sig " + ele._1 + " mods " + ele._2
                  .asInstanceOf[MethodEnvironment]
                  .modifiers + " returns " + ele._2
                  .asInstanceOf[MethodEnvironment]
                  .returnType))
         */

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
          //println("comparing ")
          //println(m1Env.signature)
          //println(m1Env.modifiers)
          //println(m2Env.signature)
          //println(m2Env.modifiers)

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

    //if (env.ast.rightSibling.isDefined) verifyAST(env, env.ast.rightSibling.get)
    //if (env.ast.leftChild.isDefined) verifyAST(env, env.ast.leftChild.get)
    env.childrenEnvironments.foreach(child => verifyEnvironment(child))
  }

  // traverses an AST as part of verifying an environment,
  // only go as deep as an environments scope (eg. dont go into methods, classes, etc...)
  def verifyAST(env: GenericEnvironment, ast: AST): Unit = {
    ast match {
      case ast: Assignment => {
        verifyAssignment(ast, env)
      }
      // TODO fill in checks
      case ast: Name =>
      /*if (env.serarchForVariable(ast.name).isEmpty) {
          println("error name in env " + env.ast.toStrTree)
          throw EnvironmentError(
            "Attempting to use undefined name: " + ast.name)
        }*/
      // TODO
      case ast: FieldAccess => // TODO
      // check methods are defined
      case ast: ClassInstanceCreation =>
        if (env.serarchForClass(ast.name).isEmpty)
          throw EnvironmentError(
            "Attempting to create instance of not found class: " + ast.name)
        return
      case ast: CompilationUnit           => return
      case ast: PackageDeclaration        => return
      case ast: FieldDeclaration          => return
      case ast: LocalVariableDeclaration  => return
      case ast: FormalParameter           => return
      case ast: InterfaceDeclaration      => return
      case ast: AbstractMethodDeclaration => return
      case ast: MethodDeclaration         => return
      case ast: MethodInvocation => {
        determineMethodInvocationType(ast, env)
      }
      case ast: ClassDeclaration       => return
      case ast: ConstructorDeclaration => return
      case ast: ForStatement           => return
      case ast: WhileStatement         => return
      // Name disamibuation
      case ast: Name => {
        determineNameType(ast, env)
      }
      case _ =>
    }

    if (ast.rightSibling.isDefined) verifyAST(env, ast.rightSibling.get)
    if (ast.leftChild.isDefined) verifyAST(env, ast.leftChild.get)
  }

  def determineType(ast: AST, env: GenericEnvironment): AbstractType = {
    ast match {
      case ast: GeneralExpression       => determineExpressionType(ast, env)
      case ast: CastExpression          => determineExpressionType(ast, env)
      case ast: ConditionalExpression   => determineExpressionType(ast, env)
      case ast: UnaryExpression         => determineExpressionType(ast, env)
      case ast: Name                    => determineNameType(ast, env)
      case ast: MethodInvocation        => determineMethodInvocationType(ast, env)
      case ast: FieldAccess             => determineFieldAccessType(ast, env)
      case ast: ArrayAccess             => determineArrayAccessType(ast, env)
      case ast: BooleanLiteral          => ast.ttype
      case ast: CharacterLiteral        => ast.ttype
      case ast: IntegerLiteral          => ast.ttype
      case ast: NullLiteral             => ast.ttype
      case ast: StringLiteral           => ast.ttype
      case ast: ArrayCreationExpression => determineType(ast.primary, env)
      case ast: PrimitiveType           => ast.ttype
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
      return types.buildTypeFromString(ttype)
    }

    val primaryType = determineType(access.primary, env)
    val klass = env.serarchForClass(primaryType.stringType).get
    val ttype = klass
      .containSet((access.identifier, None))
      .asInstanceOf[VariableEnvironment]
      .ttype
    types.buildTypeFromString(ttype)
  }

  def determineArrayAccessType(access: ArrayAccess,
                               env: GenericEnvironment): AbstractType = {
    new ArrayType(determineType(access.primary, env))
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

      val primaryType = determineType(ast.primary.get, env)
      val klass = env.serarchForClass(primaryType.stringType).get

      val method = klass.findMethodWithSignature(sig)

      if (method.isEmpty) {
        throw EnvironmentError(
          "Unable to find method for invocation with signature " + sig)
      }

      types.buildTypeFromString(method.get.returnType)
    } else { // just look up the name
      println("looking up method " + ast.name)
      val methodName = ast.name.split('.')
      val sig: Signature = (methodName.last, Some(paramsSig))

      val klass = methodName.length match {
        case 1 => {
          env.findEnclosingClass()
        }
        case _ => {
          val primaryType =
            determineType(new Name(methodName.dropRight(1).mkString(".")), env)
          env.serarchForClass(primaryType.stringType).get
        }
      }

      val method = klass.findMethodWithSignature(sig)

      if (method.isEmpty) {
        println("env " + env.ast + "\n\n\n")
        println(ast.toStrTree)
        throw EnvironmentError(
          "Unable to find method for invocation with signature " + sig)
      }

      types.buildTypeFromString(method.get.returnType)
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
          types.buildTypeFromString(ast.simpleType.get)
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
          return new StringType()
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

  def determineNameType(ast: Name, env: GenericEnvironment): AbstractType = {
    val splitName = ast.name.split('.')
    val enclosingClass = env.findEnclosingClass()
    println(
      "determining name " + ast.name + " in env " + enclosingClass.qualifiedName)

    println(
      "did find local? " + env
        .serarchForVariable(splitName.head)
        .isDefined)

    if (env
          .serarchForVariable(splitName.head)
          .isDefined) {
      ast.objectPart = Some(splitName.head)
      ast.objectType = Some(
        types.buildTypeFromString(
          env.serarchForVariable(splitName.head).get.ttype))

      ast.instanceField = Some(splitName.head.drop(1).mkString("."))
    } else if (enclosingClass.containSet
                 .contains((splitName.head, None))) {
      ast.objectPart = Some(splitName.head)
      ast.objectType = Some(
        types.buildTypeFromString(
          enclosingClass
            .containSet(splitName.head, None)
            .asInstanceOf[VariableEnvironment]
            .ttype))

      ast.instanceField = Some(splitName.head.drop(1).mkString("."))
    } else {
      var i = 1
      while (i <= splitName.length) {
        if (env
              .serarchForClass(splitName.slice(0, i).mkString("."))
              .isDefined) {
          ast.objectPart = Some(splitName.slice(0, i).mkString("."))
          ast.objectType = Some(types.buildTypeFromString(ast.objectPart.get))

          if (i + 1 < splitName.length) { // do static if still stuff left
            ast.staticField = Some(splitName(i + 1))
            val staticField = env
              .serarchForClass(ast.objectPart.get)
              .get
              .containSet(splitName(i + 1), None)
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
                  .containSet(splitName(i + 1), None)
                  .asInstanceOf[VariableEnvironment]
                  .ttype))

            if (i + 2 < splitName.length) { // do instance fields if still stuff left
              ast.instanceField = Some(
                splitName.slice(i + 2, splitName.length).mkString("."))
            }
          }
          i = splitName.length + 1 // stop looping
        }
        i += 1
      }
    }

    // TODO verify that parts of name are identified and verify they actually exist
    if (ast.staticType.isDefined) {
      val instance = env
        .serarchForClass(ast.staticType.get.stringType)
        .get
        .resolveInstanceNames(ast.instanceField.get)
      ast.instanceType = Some(types.buildTypeFromString(instance.ttype))
    } else if (ast.instanceType.isDefined) { // do it for non static fields
      val instance = env
        .serarchForClass(ast.objectType.get.stringType)
        .get
        .resolveInstanceNames(ast.instanceField.get)
      ast.instanceType = Some(types.buildTypeFromString(instance.ttype))
    }

    if (ast.instanceType.isDefined) {
      ast.instanceType.get
    } else if (ast.staticType.isDefined) {
      ast.staticType.get
    } else {
      ast.objectType.get
    }
  }

  def verifyAssignment(assignment: Assignment,
                       env: GenericEnvironment): Unit = {
    verifyAssignment(determineType(assignment.getLHS, env),
                     determineType(assignment.getRHS, env))
  }

  def verifyAssignment(ttype1: AbstractType, ttype2: AbstractType): Unit = {
    if (ttype1.equals(ttype2)) return
    if (ttype1.isInstanceOf[ShortType] && ttype2.isInstanceOf[BytesType]) return
    if (ttype1.isInstanceOf[IntType] && ttype2.isInstanceOf[CharType]) return // TODO other types of assignments
    // throw new RuntimeException("TODO: Implement " + ttype1 + " " + ttype2)
  }
}
