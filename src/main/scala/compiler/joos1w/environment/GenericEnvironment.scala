package compiler.joos1w.environment

import compiler.joos1w.ast.AST
import exceptions.EnvironmentError

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GenericEnvironment(val ast: AST,
                         val parentEnvironment: Option[GenericEnvironment] =
                           None) {
  val childrenEnvironments: ListBuffer[GenericEnvironment] =
    ListBuffer[GenericEnvironment]()

  // TODO consider having more specific tables that return specific AST types
  // packages map to a list of AST since multiple files can have same package
  val packageTable: mutable.HashMap[String, List[AST]] =
    mutable.HashMap[String, List[AST]]()
  val classTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()
  val methodTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()
  val variableTable: mutable.HashMap[String, AST] =
    mutable.HashMap[String, AST]()

  def insertLocalVariable(name: String, ast: AST): Unit = {
    if (variableTable.contains(name)) {
      println("the keys" + variableTable.keySet)
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    }
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
    if (packageTable.contains(name)) {
      packageTable += name -> (packageTable(name) :+ ast)
    } else {
      packageTable += name -> List(ast)
    }
  }

  // TODO mabye return one of the variable AST instead of generic AST, if possible
  def findLocalVariable(name: String): Option[AST] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    None
  }

  def searchForVariable(name: String): Option[AST] = {
    println("searching in env " + this + " for name: " + name)
    println("is parent " + parentEnvironment.isDefined)
    if (variableTable.contains(name)) return variableTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForVariable(name)
    None
  }

  def searchForClass(name: String): Option[AST] = {
    if (classTable.contains(name)) return classTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForClass(name)
    None
  }

  def searchForMethod(name: String): Option[AST] = {
    if (methodTable.contains(name)) return methodTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForMethod(name)
    None
  }

  def insertChild(genericEnvironment: GenericEnvironment): Unit = {
    childrenEnvironments.append(genericEnvironment)
  }
}
