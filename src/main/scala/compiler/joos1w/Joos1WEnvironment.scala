package compiler.joos1w

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.GenericEnvironment
import compiler.joos1w.environment.RootEnvironment

object Joos1WEnvironment {
  def buildEnvironment(asts: List[AST]): GenericEnvironment = {
    val rootEnvironment = new RootEnvironment(new AST(), None)

    asts.foreach(ast =>
      environment.environment.buildEnvironment(ast, Option(rootEnvironment)))
    println("verifying env...")
    environment.environment.verifyEnvironment(rootEnvironment)
    println("verifying ast...")
    asts.foreach(ast => {
      // println(ast.toStrTree)
      environment.environment.verifyAST(ast)
    })
    rootEnvironment
  }
}
