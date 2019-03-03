package compiler.joos1w.environment


import compiler.joos1w.ast.{CompilationUnit, _}
import exceptions.EnvironmentError

package object environment {
  // Identifier, Parameters
  type Signature = (String, Option[List[String]])

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
        environment.insertLocalVariable(ast.name, environment.asInstanceOf[VariableEnvironment])
      case ast: FieldDeclaration =>
        environment = new VariableEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertLocalVariable(ast.name, environment.asInstanceOf[VariableEnvironment])
      case ast: FormalParameter =>
        environment = new VariableEnvironment(ast, parentEnvironment)
        environment.insertLocalVariable(ast.name, environment.asInstanceOf[VariableEnvironment])
      // class/interfaces declaration
      case ast: ClassDeclaration =>
        environment = new ClassEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertClass(ast.identifier, environment.asInstanceOf[ClassEnvironment])
      case ast: InterfaceDeclaration =>
        environment = new ClassEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertClass(ast.identifier, environment.asInstanceOf[ClassEnvironment])
      // methods declaration
      case ast: AbstractMethodDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(ast.signature, environment.asInstanceOf[MethodEnvironment])
      case ast: MethodDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(ast.signature, environment.asInstanceOf[MethodEnvironment])
      case ast: ConstructorDeclaration =>
        environment = new MethodEnvironment(ast, parentEnvironment)
        parentEnvironment.get.insertMethod(ast.signature, environment.asInstanceOf[MethodEnvironment])
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
      // TODO I dont think we will need anything now that they all have blocks
      /*case ast: IfStatement => {
        // special case for if statement where each statement child gets its own environment
        ast.getStatementChildren.foreach(statement => {
          val env = buildEnvironment(
            statement,
            Some(new BlockEnvironment(statement, parentEnvironment)))
          parentEnvironment.get.insertChild(env)
        })
        // TODO figure out special case for IF and ELSE statements
      }*/
      case _ =>
    }

    if (environment != null && parentEnvironment.isDefined) {
      parentEnvironment.get.insertChild(environment)
    }

    // but make variables parent going forward, to make checking for not yet declared things easy
    ast match {
      case ast: LocalVariableDeclaration => parentEnvironment = Some(environment)
      case ast: FormalParameter => parentEnvironment = Some(environment)
      case _ =>
    }

    // recurse across
    if (ast.rightSibling.isDefined)
      buildEnvironment(ast.rightSibling.get, parentEnvironment)

    // recurse down
    if (ast.leftChild.isDefined) {
      if (environment != null) {
        buildEnvironment(ast.leftChild.get, Some(environment))
      } else { // if null we want to add to parent environment
        buildEnvironment(ast.leftChild.get, parentEnvironment)
      }
    }

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
      case _                       => verifyAST(env, env.ast)
    }

    // do checks on the environments themselves
    env match {
      case env: ClassEnvironment =>
        // verify there are no duplicate imported types
        val importedTypes = env.getImportSets.map(importSet => importSet._2).filter(types => types != "*")
        if (importedTypes.length != importedTypes.distinct.length) {
          throw new RuntimeException("Duplicated imported types!")
        }
      case _                     =>
    }

    //if (env.ast.rightSibling.isDefined) verifyAST(env, env.ast.rightSibling.get)
    //if (env.ast.leftChild.isDefined) verifyAST(env, env.ast.leftChild.get)
    env.childrenEnvironments.foreach(child => verifyEnvironment(child))
  }

  // traverses an AST as part of verifying an environment,
  // only go as deep as an environments scope (eg. dont go into methods, classes, etc...)
  def verifyAST(env: GenericEnvironment, ast: AST): Unit = {
    ast match {
      // TODO fill in checks
      case ast: ClassInstanceCreation =>
        if (env.serarchForClass(ast.name).isEmpty)
          throw EnvironmentError(
            "Attempting to create instance of not found class: " + ast.name)
      case ast: FieldAccess =>
      // TODO
      // check methods are defined
      case ast: MethodInvocation =>
      // Stop recursing since this is a new environment
      case ast: CompilationUnit           => return
      case ast: PackageDeclaration        => return
      case ast: FieldDeclaration          => return
      case ast: LocalVariableDeclaration  => return
      case ast: FormalParameter           => return
      case ast: InterfaceDeclaration      => return
      case ast: AbstractMethodDeclaration => return
      case ast: MethodDeclaration         => return
      case ast: ClassDeclaration =>
        //println("some classes super set " + ast.getSuperSet)
        return
      case ast: ConstructorDeclaration => return
      case ast: ForStatement           => return
      case ast: WhileStatement         => return
      case _                           =>
    }

    if (ast.rightSibling.isDefined) verifyAST(env, ast.rightSibling.get)
    if (ast.leftChild.isDefined) verifyAST(env, ast.leftChild.get)
  }
}
