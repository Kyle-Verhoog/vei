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

  protected def searchForSimpleVariable(name: String): Option[AST] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleVariable(name)
    None
  }

  protected def searchForSimpleClass(name: String): Option[AST] = {
    if (classTable.contains(name)) return classTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleClass(name)
    None
  }

  protected def searchForSimpleMethod(sig: Signature): Option[AST] = {
    if (methodTable.contains(sig)) return methodTable.get(sig)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleMethod(sig)
    None
  }

  def findPackageEnv(name: String): Option[PackageEnvironment] = {
    parentEnvironment.get.findPackageEnv(name)
  }

  protected def searchForQualifiedClass(name: String): Option[AST] = {
    val splitName = name.split('.')
    if (splitName.length < 2)
      throw new RuntimeException("Class name is not qualified!")

    val pkgName = splitName.dropRight(1).mkString(".")
    val className = splitName.last

    val pkg = findPackageEnv(pkgName)
    if (pkg.isEmpty) throw new RuntimeException("Package: " + pkg + " not found")
    pkg.get.searchForSimpleClass(className)
  }

  protected def searchForQualifiedVariable(name: String): Option[AST] = {
    throw new RuntimeException("TODO") // TODO
  }

  protected def searchForQualifiedMethod(sig: Signature): Option[AST] = {
    throw new RuntimeException("TODO") // TODO
  }

  def serarchForVariable(name: String): Option[AST] = {
    if (name.contains('.')) {
      return searchForQualifiedVariable(name)
    }
    searchForSimpleVariable(name)
  }

  def serarchForClass(name: String): Option[AST] = {
    if (name.contains('.')) {
      return searchForQualifiedClass(name)
    }
    searchForSimpleClass(name)
  }

  def serarchForMethod(sig: Signature): Option[AST] = {
    if (sig._1.contains('.')) {
      return searchForQualifiedMethod(sig)
    }
    searchForSimpleMethod(sig)
  }

  def createOrReturnRootPackageEnv(name: String): PackageEnvironment = {
    parentEnvironment.get.createOrReturnRootPackageEnv(name)
  }

  def retrieveAllClassesInPackage(name: String): Map[String, AST] = {
    parentEnvironment.get.retrieveAllClassesInPackage(name)
  }

  def insertChild(genericEnvironment: GenericEnvironment): Unit = {
    childrenEnvironments.append(genericEnvironment)
  }
}
