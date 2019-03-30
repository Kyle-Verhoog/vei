package compiler.joos1w.environment

import compiler.joos1w.ast.{
  AST,
  ClassDeclaration,
  FieldDeclaration,
  InterfaceDeclaration
}
import compiler.joos1w.environment.environment.Signature
import exceptions.EnvironmentError

import scala.collection.mutable

class ClassEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  myAst match {
    case ast: ClassDeclaration => {
      insertClass(ast.identifier, this)

      if (superSet.contains(ast.identifier)) {
        throw EnvironmentError(
          "Class: " + ast.identifier + " cannot extend or implement itself!")
      }

      if (singleTypeImports.contains(ast.identifier) && singleTypeImports(
            ast.identifier) != qualifiedName) {
        throw EnvironmentError("Cannot import type of same name!")
      }
    }
    case ast: InterfaceDeclaration => {
      insertClass(ast.identifier, this)

      if (superSet.contains(ast.identifier)) {
        throw EnvironmentError(
          "Interface: " + ast.identifier + " cannot extend or implement itself!")
      }

      if (singleTypeImports.contains(ast.identifier) && singleTypeImports(
            ast.identifier) != qualifiedName) {
        throw EnvironmentError("Cannot import interface of same name!")
      }
    }
    case _ =>
      throw new RuntimeException("Class env needs class or interface as AST")
  }

  def packageName: String = {
    parentEnvironment.get match {
      case env: PackageEnvironment => env.pkgName
      case _ =>
        throw new RuntimeException(
          "expecting parent of class env to be pkg env")
    }
  }

  def qualifiedName: String = {
    ast match {
      case ast: ClassDeclaration     => packageName + "." + ast.identifier
      case ast: InterfaceDeclaration => packageName + "." + ast.identifier
    }
  }

  // gets all extends recursively upward, with lower envs to compare against for cycles
  def verifyNoCyclesInExtends(
      previousExtends: List[ClassEnvironment] = List()): Unit = {
    superSetClasses.foreach(klass => {
      if (previousExtends.contains(klass))
        throw EnvironmentError("Cycle found with class " + klass.qualifiedName)

      klass.verifyNoCyclesInExtends(previousExtends :+ klass)
    })
  }
  def superSet: List[String] = {
    if (qualifiedName == "java.lang.Object" || qualifiedName == "java.lang.AbstractKevin")
      return List()

    myAst match {
      case ast: ClassDeclaration => {
        ast.getSuperSet
      }
      case ast: InterfaceDeclaration => {
        ast.getExtends
      }
    }
  }

  def isSubClassOf(superClass: ClassEnvironment): Boolean = {
    if (superClass.qualifiedName == "java.lang.Object") return true
    if (superClass == this) return true
    if (superSetClasses.contains(superClass)) return true
    superSetClasses
      .map(klass => klass.isSubClassOf(superClass))
      .exists(ans => ans)
  }

  def superSetClasses: List[ClassEnvironment] = {
    superSet.map(superClass => {
      serarchForClass(superClass).get
    })
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

  def getPublicInheritedSet: Map[Signature, GenericEnvironment] = {
    inheritSet.filter(entry => {
      entry._2 match {
        case env: MethodEnvironment   => env.modifiers.contains("public")
        case env: VariableEnvironment => env.modifiers.contains("public")
      }
    })
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

  def verifySingleTypeImportsExist(): Unit = {
    singleTypeImports.foreach(importSet => {
      if (searchForQualifiedClass(importSet._2).isEmpty) {
        throw EnvironmentError(
          "Single type import does not exist: " + importSet)
      }
    })
  }

  def inheritSet: Map[Signature, GenericEnvironment] = {
    val map = mutable.Map[Signature, GenericEnvironment]()

    // special case for java.lang.Object, it cant inherit anything since it is the root
    if (qualifiedName == "java.lang.Object" || qualifiedName == "java.lang.AbstractKevin")
      return map.toMap

    superSetClasses.foreach(classEnv => {
      classEnv.containSet.foreach(entry => {
        val signature = entry._1
        val env = entry._2
        //if (map.contains(entry._1)) throw new RuntimeException("duplicate ")

        // check that we should inherit this
        env match {
          case e: MethodEnvironment =>
            if (nodecl(signature)) {
              // only inherit abstract mmethod if its in allabs
              if (e.modifiers.contains("abstract")) {
                // only inherit abstract mmethod if its in allabs
                if (allabs(signature)) map += signature -> env
              } else { // non abstract method can be inherited
                map += signature -> env
              }
            }
          case e: VariableEnvironment =>
            // only inherit field if it is not declared in this environment
            if (nodecl(signature)) map += entry._1 -> entry._2
        }
      })
    })

    map.toMap
  }

  def staticFields: List[VariableEnvironment] = {
    containSet.values.foldLeft(List[VariableEnvironment]())((acc, env) => {
      env match {
        case v: VariableEnvironment =>
          v.myAst match {
            case f: FieldDeclaration =>
              if (f.modifiers.contains("static")) v :: acc else acc
            case _ => acc
          }
        case _ => acc
      }
    })
  }

  def fields: List[VariableEnvironment] = {
    containSet.values.foldLeft(List[VariableEnvironment]())((acc, env) => {
      env match {
        case v: VariableEnvironment =>
          v.myAst match {
            case _: FieldDeclaration => v :: acc
            case _                   => acc
          }
        case _ => acc
      }
    })
  }

  def allFields: List[VariableEnvironment] = {
    containSet.values.foldLeft(List[VariableEnvironment]())((acc, env) => {
      env match {
        case v: VariableEnvironment => v :: acc
        case _                      => acc
      }
    })
  }

  def numFields: Integer = {
    fields.length
  }

  def nodecl(signature: Signature): Boolean = {
    !declareSet.contains(signature)
  }

  def allabs(signature: Signature): Boolean = {
    var bool = true
    superSetClasses.foreach(classEnv => {
      classEnv.containSet.foreach(entry => {
        val sig = entry._1
        val env = entry._2
        if (env.isInstanceOf[MethodEnvironment] && signature == sig) {
          bool = bool && env
            .asInstanceOf[MethodEnvironment]
            .modifiers
            .contains("abstract")
        }
      })
    })

    bool
  }

  def findDeclaredMethod(signature: Signature): Option[MethodEnvironment] = {
    val declared = declareSet.get(signature)
    if (declared.isEmpty) return None

    declared.get match {
      case e: MethodEnvironment => Option(e)
      case _                    => None
    }
  }

  // NOTE order matters within the replaced elements!!!
  def replaceSet: List[(MethodEnvironment, MethodEnvironment)] = {
    var replaced = List[(MethodEnvironment, MethodEnvironment)]()

    val superClasses = superSetClasses

    for (i <- superClasses.indices) {
      val klass1 = superClasses(i)
      for (m1 <- klass1.containSet.filter(e =>
             e._2.isInstanceOf[MethodEnvironment])) {
        val m1Sig = m1._1
        val m1Env = m1._2.asInstanceOf[MethodEnvironment]

        // add it to the replace set if needed
        val declaredMethod = findDeclaredMethod(m1Sig)
        //println("considering replacing for method in class " + m1Sig)
        if (declaredMethod.isDefined && declaredMethod.get.signature == m1Sig) {
          replaced = replaced :+ (declaredMethod.get, m1Env)
        }

        // check against all other super classes and their methods
        for (j <- superClasses.indices) {
          if (i != j) { // dont want to compare same class
            val klass2 = superClasses(j)
            for (m2 <- klass2.containSet.filter(e =>
                   e._2.isInstanceOf[MethodEnvironment])) {
              val m2Sig = m2._1
              val m2Env = m2._2.asInstanceOf[MethodEnvironment]

              if (!m1Env.modifiers.contains("abstract") && m2Env.modifiers
                    .contains("abstract") && m1Sig == m2Sig) {
                replaced = replaced :+ (m1Env, m2Env)
              }
            }
          }
        }
      }
    }

    replaced
  }

  def containSet: Map[Signature, GenericEnvironment] = {
    val map = mutable.Map[Signature, GenericEnvironment]()

    inheritSet.foreach(entry => map += entry._1 -> entry._2)
    declareSet.foreach(entry => map += entry._1 -> entry._2)
    map.toMap
  }

  def normalizeSignature(sig: Signature): Signature = {
    val params = sig._2.get.asInstanceOf[List[String]]
    val newParams =
      params.map(param => {
        val paramClass = serarchForClass(param)
        if (paramClass.isDefined) {
          paramClass.get.qualifiedName
        } else { // primitive case
          param
        }
      })
    (sig._1, Option(newParams))
  }

  def findMethodWithSignature(sig: Signature): Option[MethodEnvironment] = {
    containSet.values.foreach(entry => {
      entry match {
        case method: MethodEnvironment => {
          // println("looking at sig " + method.signature)
          if (normalizeSignature(method.signature) == sig) {
            return Some(method)
          }
        }
        case _ =>
      }
    })
    None
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
      case ast: InterfaceDeclaration => {
        (ast.getImports.map(imp => {
          val partsOfName = imp.name.split('.')
          (partsOfName.dropRight(1).mkString("."), partsOfName.last)
        }) ++ List(("java.lang", "*"))).distinct
      }
    }
  }

  def lookUpImportedClass(klass: String): Option[ClassEnvironment] = {
    var imported = mutable.Map[String, ClassEnvironment]()

    getImportSets.foreach(importSet => {
      val packageName = importSet._1
      val className = importSet._2

      if (className == "*") { // look up all sub classes
        retrieveAllClassesInPackage(packageName).foreach(pair => {
          if (imported.contains(pair._1) && klass == pair._1)
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

        if (imported.contains(className) && klass == className)
          throw new RuntimeException(
            "duplicate class names imported: " + className)

        imported += className -> classAST.get
      }
    })

    imported.get(klass)
  }

  // takes in a qualified or simple instance name and resolves it
  // also checks if it is accessible from the given env, and if not
  // throws env error
  def resolveInstanceNames(
      name: String,
      callingEnv: ClassEnvironment): VariableEnvironment = {
    println("checking in class " + ast)
    val splitName = name.split('.')
    val env = containSet.get(splitName.head, None)

    if (env.isEmpty) {
      throw EnvironmentError(
        "attempting to resolve unknown instance name: " + name + " within env " + qualifiedName)
    }

    val varEnv = env.get.asInstanceOf[VariableEnvironment]

    if (splitName.length == 1) {
      val modifiers = varEnv.modifiers
      if (modifiers.contains("protected") && !callingEnv.isSubClassOf(this)) {
        throw EnvironmentError(
          "Attempting to resolve protected instance name in non-super class! Name: " + name + " env: " + callingEnv.ast)
      } else if (modifiers.contains("private") && callingEnv != this) {
        throw EnvironmentError(
          "Attempting to resolve private instance name in non-super class!")
      }
      return varEnv
    }
    varEnv
      .findEnclosingClass()
      .resolveInstanceNames(splitName.drop(1).mkString("."),
                            varEnv.findEnclosingClass())
  }

  // takes in a qualified or simple instance name and resolves it
  def resolveInstanceNames(name: String): VariableEnvironment = {
    val splitName = name.split('.')
    val env = containSet.get(splitName.head, None)

    if (env.isEmpty) {
      throw EnvironmentError(
        "attempting to resolve unknown instance name: " + name + " within env " + this.ast)
    }

    if (splitName.length == 1) return env.get.asInstanceOf[VariableEnvironment]
    env.get
      .asInstanceOf[VariableEnvironment]
      .findEnclosingClass()
      .resolveInstanceNames(splitName.drop(1).mkString("."))
  }

  def verifyImportedPackagsExist(): Unit = {
    getImportSets.foreach(importSet => {
      if (findPackageEnv(importSet._1).isEmpty) {
        throw EnvironmentError("Imported package not found " + importSet._1)
      }
    })
  }

  def verifyNoPackageCollidesWithName(): Unit = {
    if (findPackageEnv(qualifiedName).isDefined) {
      throw EnvironmentError(
        "Package exists with class/interface name: " + qualifiedName)
    }
  }

  def verifyAbstractProperties(): Unit = {
    ast match {
      // verify classes with abstract methods are abstract
      case ast: ClassDeclaration => {
        containSet.values.foreach(env => {
          env match {
            case e: MethodEnvironment =>
              if (e.modifiers.contains("abstract") && !ast.modifiers.contains(
                    "abstract")) {
                throw EnvironmentError(
                  "Class " + qualifiedName + " is not abstract but has abstract method " + e.signature)
              }

            case _ =>
          }
        })
      }
      case _ =>
    }
  }

  def verifyImplementsAreInterfaces(): Unit = {
    ast match {
      case klass: ClassDeclaration =>
        val interfaces =
          klass.getInterfaces.map(interface => serarchForClass(interface).get)
        if (interfaces.distinct.length != interfaces.length) {
          throw EnvironmentError("Duplicate interfaces implemented")
        }

        interfaces.foreach(interface => {
          if (!interface.ast
                .isInstanceOf[InterfaceDeclaration]) {
            throw EnvironmentError(
              "Using class " + interface + " as interface!")
          }
        })
      case _ =>
    }
  }

  def verifyDontExtendFinalClass(): Unit = {
    superSetClasses.foreach(klass => {
      klass.ast match {
        case k: ClassDeclaration =>
          if (k.modifiers.contains("final")) {
            throw EnvironmentError("Cannot extend final class!")
          }
        case _ =>
      }
    })
  }

  override def searchForSimpleClass(name: String): Option[ClassEnvironment] = {
    // search enclosing class/interface
    if (classTable.contains(name)) return classTable.get(name)

    // search type imports
    if (singleTypeImports.contains(name)) {
      return searchForQualifiedClass(singleTypeImports(name))
    }

    // search packages
    if (parentEnvironment.get.searchForSimpleClass(name).isDefined)
      return parentEnvironment.get.searchForSimpleClass(name)

    // search imports
    lookUpImportedClass(name)
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

  override def findEnclosingMethod(): MethodEnvironment = {
    throw new RuntimeException("Cannot find enclosing method on a class env!")
  }

  override def findEnclosingClass(): ClassEnvironment = {
    this
  }

  override def isInMethod(): Boolean = false

  // overide to add ordering information to fields
  override def insertLocalVariable(name: String,
                                   env: VariableEnvironment): Unit = {
    if (variableTable.contains(name)) {
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    }

    env.order = variableTable.keys.size
    variableTable += name -> env
    if (parentEnvironment.isDefined) parentEnvironment.get.verifyVariable(name)
  }
}
