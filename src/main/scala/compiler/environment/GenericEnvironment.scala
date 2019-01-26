package compiler.environment

import compiler.ast.AST

import scala.collection.mutable

abstract class GenericEnvironment(
    val parentEnvironment: Option[GenericEnvironment] = None) {
  // mapping from a name to an AST
  val symbolTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()

  def findLocalSymbol(name: String): Unit = {

  }
}
