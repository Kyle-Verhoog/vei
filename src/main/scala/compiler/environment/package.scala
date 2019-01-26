package compiler

import compiler.ast.{AST, CompilationUnit, VariableDeclarator}

package object environment {
  def buildEnvironment(ast: AST, parentEnvironment: GenericEnvironment): GenericEnvironment = {
    var environment: GenericEnvironment = null // TODO null?

    ast match {
      case ast: CompilationUnit =>
        environment = new RootEnvironment
      case ast: VariableDeclarator =>
        parentEnvironment
    }

    environment
  }
}
