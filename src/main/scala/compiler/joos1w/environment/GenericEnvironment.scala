package compiler.joos1w.environment

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.environment.Signature
import exceptions.EnvironmentError

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GenericEnvironment(val ast: AST,
                         val parentEnvironment: Option[GenericEnvironment] =
                           None) {
  val childrenEnvironments: ListBuffer[GenericEnvironment] =
    ListBuffer[GenericEnvironment]()

  // TODO consider having more specific tables that return specific AST types
  val classTable: mutable.HashMap[String, AST] = mutable.HashMap[String, AST]()
  val methodTable: mutable.HashMap[Signature, AST] =
    mutable.HashMap[Signature, AST]()
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

  def insertMethod(sig: Signature, ast: AST): Unit = {
    if (methodTable.contains(sig))
      throw EnvironmentError(
        "Method: " + sig + " already declared in current scope")
    methodTable += sig -> ast
  }

  def insertClass(name: String, ast: AST): Unit = {
    if (classTable.contains(name))
      throw EnvironmentError(
        "Class: " + name + " already declared in current scope")
    classTable += name -> ast
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

  def searchForMethod(sig: Signature): Option[AST] = {
    if (methodTable.contains(sig)) return methodTable.get(sig)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForMethod(sig)
    None
  }

  def findPackageEnv(name: String): Option[PackageEnvironment] = {
    parentEnvironment.get.findPackageEnv(name)
  }

  def createOrReturnRootPackageEnv(name: String): PackageEnvironment = {
    parentEnvironment.get.createOrReturnRootPackageEnv(name)
  }

  def retrieveAllClassesInPackage(name: String): Map[String, AST] = {
    println("searching up")
    parentEnvironment.get.retrieveAllClassesInPackage(name)
  }

  def insertChild(genericEnvironment: GenericEnvironment): Unit = {
    childrenEnvironments.append(genericEnvironment)
  }
}
