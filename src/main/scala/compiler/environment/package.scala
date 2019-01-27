package compiler

import compiler.ast._

package object environment {
  def buildEnvironment(ast: AST,
                       parent: GenericEnvironment): GenericEnvironment = {
    var environment: GenericEnvironment = null // TODO null?
    var parentEnvironment = parent;

    // TODO finish
    ast match {
      case ast: CompilationUnit =>
        environment = new RootEnvironment
      // packages
      case ast: PackageDeclaration =>
        environment = new PackageEnvironment
        parentEnvironment.insertPackage(ast.name, ast)
      // variables
      case ast: VariableDeclarator =>
        environment = new VariableEnvironment
        parentEnvironment.insertLocalVariable(ast.name, ast)
      case ast: FormalParameter =>
        environment = new VariableEnvironment
        parentEnvironment.insertLocalVariable(ast.name, ast)
      // class/interfaces
      case ast: ClassDeclaration =>
        environment = new ClassEnvironment
        parentEnvironment.insertClass(ast.identifier, ast)
      case ast: InterfaceDeclaration =>
        environment = new ClassEnvironment
        parentEnvironment.insertClass(ast.identifier, ast)
      // methods
      case ast: AbstractMethodDeclaration =>
        environment = new MethodEnvironment
        parentEnvironment.insertMethod(ast.identifier, ast)
      case ast: MethodDeclaration =>
        environment = new MethodEnvironment
        parentEnvironment.insertMethod(ast.identifier, ast)
      case ast: ConstructorDeclaration =>
        environment = new MethodEnvironment
        parentEnvironment.insertMethod(ast.identifier, ast)
      // other blocks (for, while, etc...)
      case ast: ForStatement =>
        environment = new BlockEnvironment
      case ast: WhileStatement =>
        environment = new BlockEnvironment
      case _ =>
    }

    // if this is package declaration, then it will be the parent
    if (ast.isInstanceOf[PackageDeclaration]) {
      parentEnvironment = environment
    }

    // recurse across
    if (ast.rightSibling.isDefined)
      buildEnvironment(ast.rightSibling.get, parentEnvironment)

    // recurse down
    if (ast.leftChild.isDefined) {
      if (environment != null) {
        buildEnvironment(ast.leftChild.get, environment)
      } else { // if null we want to add to parent environment
        buildEnvironment(ast.leftChild.get, parentEnvironment)
      }
    }

    environment
  }
}
