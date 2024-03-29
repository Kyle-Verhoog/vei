package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, LocalVariableDeclaration}
import compiler.joos1w.environment.environment.Signature
import exceptions.EnvironmentError

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GenericEnvironment(val ast: AST,
                         val parentEnvironment: Option[GenericEnvironment] =
                           None) {
  val childrenEnvironments: ListBuffer[GenericEnvironment] =
    ListBuffer[GenericEnvironment]()
  var fpOffset = 0
  var localVarOffset = 0

  // TODO consider having more specific tables that return specific AST types
  val classTable: mutable.HashMap[String, ClassEnvironment] =
    mutable.HashMap[String, ClassEnvironment]()
  val methodTable: mutable.HashMap[Signature, MethodEnvironment] =
    mutable.HashMap[Signature, MethodEnvironment]()
  val variableTable: mutable.HashMap[String, VariableEnvironment] =
    mutable.HashMap[String, VariableEnvironment]()

  def verifyVariable(name: String): Unit = {
    if (variableTable.contains(name)) {
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    }

    if (parentEnvironment.isDefined) parentEnvironment.get.verifyVariable(name)
  }

  def insertLocalVariable(name: String, env: VariableEnvironment): Unit = {
    if (variableTable.contains(name)) {
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    }
    env.myAst match {
      case _: LocalVariableDeclaration =>
        val method = findEnclosingMethod()
        localVarOffset = method.localVarCount
        method.localVarCount += 1
    }
    variableTable += name -> env
    if (parentEnvironment.isDefined) parentEnvironment.get.verifyVariable(name)
  }

  def insertMethod(sig: Signature, env: MethodEnvironment): Unit = {
    if (methodTable.contains(sig))
      throw EnvironmentError(
        "Method: " + sig + " already declared in current scope")
    methodTable += sig -> env
  }

  def insertClass(name: String, env: ClassEnvironment): Unit = {
    if (classTable.contains(name))
      throw EnvironmentError(
        "Class: " + name + " already declared in current scope")

    classTable += name -> env
  }

  // TODO mabye return one of the variable AST instead of generic AST, if possible
  def findLocalVariable(name: String): Option[VariableEnvironment] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    None
  }

  protected def searchForSimpleVariable(
      name: String): Option[VariableEnvironment] = {
    // println("looking at var table " + variableTable.keys)
    if (variableTable.contains(name)) return variableTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleVariable(name)
    None
  }

  def searchForSimpleClass(name: String): Option[ClassEnvironment] = {
    if (classTable.contains(name)) return classTable.get(name)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleClass(name)
    None
  }

  protected def searchForSimpleMethod(
      sig: Signature): Option[MethodEnvironment] = {
    if (methodTable.contains(sig)) return methodTable.get(sig)
    if (parentEnvironment.isDefined)
      return parentEnvironment.get.searchForSimpleMethod(sig)
    None
  }

  def findPackageEnv(name: String): Option[PackageEnvironment] = {
    parentEnvironment.get.findPackageEnv(name)
  }

  protected def searchForQualifiedClass(
      name: String): Option[ClassEnvironment] = {
    val splitName = name.split('.')
    if (splitName.length < 2)
      throw new RuntimeException("Class name is not qualified!")

    val pkgName = splitName.dropRight(1).mkString(".")
    val className = splitName.last

    val pkg = findPackageEnv(pkgName)
    if (pkg.isEmpty)
      throw new RuntimeException("Package: " + pkgName + " not found")
    pkg.get.searchForSimpleClass(className)
  }

  protected def searchForQualifiedVariable(
      name: String): Option[VariableEnvironment] = {
    throw new RuntimeException(s"TODO $name") // TODO
  }

  protected def searchForQualifiedMethod(
      sig: Signature): Option[MethodEnvironment] = {
    throw new RuntimeException(s"TODO $sig") // TODO
  }

  def serarchForVariable(name: String): Option[VariableEnvironment] = {
    if (name.contains('.')) {
      return searchForQualifiedVariable(name)
    }
    searchForSimpleVariable(name)
  }

  def serarchForClass(name: String): Option[ClassEnvironment] = {
    // println("searching for class " + name)
    if (name.contains('.')) {
      if (serarchForClass(name.split('.').dropRight(1).mkString(".")).isDefined) {
        throw EnvironmentError("prefix of a type cannot itself be a type!")
      }
      return searchForQualifiedClass(name)
    }
    searchForSimpleClass(name)
  }

  def serarchForMethod(sig: Signature): Option[MethodEnvironment] = {
    if (sig._1.contains('.')) {
      return searchForQualifiedMethod(sig)
    }
    searchForSimpleMethod(sig)
  }

  def createOrReturnRootPackageEnv(name: String): PackageEnvironment = {
    parentEnvironment.get.createOrReturnRootPackageEnv(name)
  }

  def retrieveAllClassesInPackage(
      name: String): Map[String, ClassEnvironment] = {
    parentEnvironment.get.retrieveAllClassesInPackage(name)
  }

  def insertChild(genericEnvironment: GenericEnvironment): Unit = {
    childrenEnvironments.append(genericEnvironment)
  }

  def findEnclosingClass(): ClassEnvironment = {
    parentEnvironment.get.findEnclosingClass()
  }

  def findEnclosingMethod(): MethodEnvironment = {
    parentEnvironment.get.findEnclosingMethod()
  }

  def isInMethod(): Boolean = {
    parentEnvironment.get.isInMethod()
  }

  // does the same thing as serarchClass but without checking for prefix
  // since this is to be used for typechecking
  def lookupClass(name: String): Option[ClassEnvironment] = {
    // println("searching for class " + name)
    if (name.contains('.')) {
      return searchForQualifiedClass(name)
    }
    searchForSimpleClass(name)
  }
}
