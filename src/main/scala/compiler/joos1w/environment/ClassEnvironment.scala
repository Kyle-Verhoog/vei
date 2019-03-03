package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, ClassDeclaration, InterfaceDeclaration}
import compiler.joos1w.environment.environment.Signature
import exceptions.EnvironmentError

import scala.collection.mutable

class ClassEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  myAst match {
    case ast: ClassDeclaration     => insertClass(ast.identifier, this)
    case ast: InterfaceDeclaration => insertClass(ast.identifier, this)
    case _ =>
      throw new RuntimeException("Class env needs class or interface as AST")
  }

  def superSet: List[String] = {
    myAst match {
      case ast: ClassDeclaration => {
        if (ast.getSuperSet.contains(ast.identifier)) {
          throw EnvironmentError(
            "Class: " + ast.identifier + " cannot extend or implement itself!")
        }
        ast.getSuperSet
      }
      case ast: InterfaceDeclaration => {
        if (ast.getExtends.contains(ast.identifier)) {
          throw EnvironmentError(
            "Class: " + ast.identifier + " cannot extend or implement itself!")
        }
        ast.getExtends
      }
    }
  }

  def declareSet: Map[Signature, GenericEnvironment] = {
    val map = mutable.Map[Signature, GenericEnvironment]()
    childrenEnvironments
      .foreach(child =>
        child match {
          case c: MethodEnvironment   => map += c.signature -> c
          case c: VariableEnvironment => map += (c.name, None) -> c
          case _                      =>
      })
    map.toMap
  }

  // mapping from the type to the fully qualified name
  // eg. C -> A.B.C
  def singleTypeImports: Map[String, String] = {
    val map = mutable.Map[String, String]()
    getImportSets
      .filter(importSet => importSet._2 != "*")
      .foreach(importSet =>
        map += importSet._2 -> (importSet._1 + "." + importSet._2))
    map.toMap
  }

  def inheritSet: Map[Signature, GenericEnvironment] = {
    val map = mutable.Map[Signature, GenericEnvironment]()

    superSet.foreach(superClass => {
      getImportedClasses(superClass).declareSet
        .foreach(entry => {
          if (map.contains(entry._1)) throw new RuntimeException("duplicate ")
          map += entry._1 -> entry._2
        })
    })
    map.toMap
  }

  def containSet: Map[Signature, GenericEnvironment] = {
    val map = mutable.Map[Signature, GenericEnvironment]()

    inheritSet.foreach(entry => map += entry._1 -> entry._2)
    declareSet.foreach(entry => map += entry._1 -> entry._2)
    map.toMap
  }

  // returns set of packages and thing to be imported
  // eg. (a.b, c), (j.k, *)
  def getImportSets: List[(String, String)] = {
    myAst match {
      case ast: ClassDeclaration => {
        (ast.getImports.map(imp => {
          val partsOfName = imp.name.split('.')
          (partsOfName.dropRight(1).mkString("."), partsOfName.last)
        }) ++ List(("java.lang", "*"))).distinct
      }
      case ast: InterfaceDeclaration => List(("java.lang", "*")) // TODO
    }
  }

  def getImportedClasses: Map[String, ClassEnvironment] = {
    var imported = mutable.Map[String, ClassEnvironment]()

    getImportSets.foreach(importSet => {
      val packageName = importSet._1
      val className = importSet._2

      if (className == "*") { // look up all sub classes
        retrieveAllClassesInPackage(packageName).foreach(pair => {
          if (imported.contains(pair._1))
            throw new RuntimeException(
              "duplicate class names imported: " + pair._1)
          imported += pair
        })

      } else { // just find the class
        val pkg = findPackageEnv(importSet._1)
        if (pkg.isEmpty)
          throw new RuntimeException("couldnt find package " + packageName)

        val classAST = pkg.get.classTable.get(className)
        if (classAST.isEmpty)
          throw new RuntimeException(
            "could not find class " + className + " in package " + packageName)

        if (imported.contains(className))
          throw new RuntimeException(
            "duplicate class names imported: " + className)

        imported += className -> classAST.get
      }
    })

    imported.toMap
  }

  override def searchForSimpleClass(name: String): Option[ClassEnvironment] = {
    // search enclosing class/interface
    if (classTable.contains(name)) return classTable.get(name)

    // search type imports
    if (singleTypeImports.contains(name))
      return searchForQualifiedClass(singleTypeImports(name))

    // search packages
    if (parentEnvironment.get.searchForSimpleClass(name).isDefined)
      return parentEnvironment.get.searchForSimpleClass(name)

    // search imports
    getImportedClasses.get(name)
  }

  override def searchForSimpleMethod(
      sig: Signature): Option[MethodEnvironment] = {
    if (methodTable.contains(sig)) return methodTable.get(sig)
    inheritSet.get(sig).asInstanceOf[Option[MethodEnvironment]]
  }

  override def searchForSimpleVariable(
      name: String): Option[VariableEnvironment] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    inheritSet.get((name, None)).asInstanceOf[Option[VariableEnvironment]]
  }

}
