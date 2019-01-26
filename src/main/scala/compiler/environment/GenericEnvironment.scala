package compiler.environment

import compiler.ast.AST
import exceptions.EnvironmentError

import scala.collection.mutable

class GenericEnvironment(
    val parentEnvironment: Option[GenericEnvironment] = None) {
  // TODO consider having more specific tables that return specific AST types
  val packageTable: mutable.HashMap[String, AST] =
    mutable.HashMap[String, AST]()
  val classTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()
  val methodTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()
  val variableTable: mutable.HashMap[String, AST] =
    mutable.HashMap[String, AST]()

  def insertLocalVariable(name: String, ast: AST): Unit = {
    if (variableTable.contains(name))
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    variableTable += name -> ast
  }

  def insertMethod(name: String, ast: AST): Unit = {
    if (methodTable.contains(name))
      throw EnvironmentError(
        "Method: " + name + " already declared in current scope")
    methodTable += name -> ast
  }

  def insertClass(name: String, ast: AST): Unit = {
    if (classTable.contains(name))
      throw EnvironmentError(
        "Class: " + name + " already declared in current scope")
    classTable += name -> ast
  }

  def insertPackage(name: String, ast: AST): Unit = {
    if (packageTable.contains(name))
      throw EnvironmentError(
        "Package: " + name + " already declared in current scope")
    packageTable += name -> ast
  }

  // TODO mabye return one of the variable AST instead of generic AST, if possible
  def findLocalVariable(name: String): Option[AST] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    None
  }

  def searchForVariable(name: String): Option[AST] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    if (parentEnvironment.isDefined) return searchForVariable(name)
    None
  }

  def searchForClass(name: String): Option[AST] = {
    if (classTable.contains(name)) return classTable.get(name)
    if (parentEnvironment.isDefined) return searchForClass(name)
    None
  }

  def searchForMethod(name: String): Option[AST] = {
    if (methodTable.contains(name)) return methodTable.get(name)
    if (parentEnvironment.isDefined) return searchForMethod(name)
    None
  }
}
