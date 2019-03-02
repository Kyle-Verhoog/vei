package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, ClassDeclaration, InterfaceDeclaration}
import compiler.joos1w.environment.environment.Signature

import scala.collection.mutable

class ClassEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  myAst match {
    case ast: ClassDeclaration     =>
    case ast: InterfaceDeclaration =>
    case _ =>
      throw new RuntimeException("Class env needs class or interface as AST")
  }

  def superSet: List[String] = {
    myAst match {
      case ast: ClassDeclaration     => ast.getSuperSet
      case ast: InterfaceDeclaration => List("TODO") // TODO
    }
  }

  def declareSet: Map[Signature, AST] = {
    myAst match {
      case ast: ClassDeclaration     => ast.getDeclareSet
      case ast: InterfaceDeclaration => Map() // TODO
    }
  }

  def inheritSet: Map[Signature, AST] = {
    val map = mutable.Map[Signature, AST]()

    superSet.foreach(superClass => {
      getImportedClasses(superClass)
        .asInstanceOf[ClassDeclaration]
        .getDeclareSet
        .foreach(entry => {
          if (map.contains(entry._1)) throw new RuntimeException("duplicate ")
          map += entry._1 -> entry._2
        })
    })
    map.toMap
  }

  def containSet: Map[Signature, AST] = {
    val map = mutable.Map[Signature, AST]()

    inheritSet.foreach(entry => map += entry._1 -> entry._2)
    declareSet.foreach(entry => map += entry._1 -> entry._2)
    map.toMap
  }

  // returns set of packages and thing to be imported
  // eg. (a.b, c), (j.k, *)
  def getImportSets: List[(String, String)] = {
    myAst match {
      case ast: ClassDeclaration => {
        ast.getImports.map(imp => {
          val partsOfName = imp.name.split('.')
          (partsOfName.dropRight(1).mkString("."), partsOfName.last)
        }) ++ List(("java.lang", "*"))
      }
      case ast: InterfaceDeclaration => List(("java.lang", "*")) // TODO
    }
  }

  def getImportedClasses: Map[String, AST] = {
    var imported = mutable.Map[String, AST]()

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

  override def searchForSimpleClass(name: String): Option[AST] = {
    if (classTable.contains(name)) return classTable.get(name)
    getImportedClasses.get(name)
  }

  override def searchForSimpleMethod(sig: Signature): Option[AST] = {
    if (methodTable.contains(sig)) return methodTable.get(sig)
    inheritSet.get(sig)
  }

  override def searchForSimpleVariable(name: String): Option[AST] = {
    if (variableTable.contains(name)) return variableTable.get(name)
    inheritSet.get(name, List())
  }

}
