package compiler.joos1w.env

import compiler.joos1w.ast.{AST, ClassDeclaration, PackageDeclaration}

/*
object Package {
  def fromAST(ast: PackageDeclaration): Package = {
    val clsNodes = getClassASTNodes(Some(ast))
    new Package(
      clsNodes,
    )
  }

  def getClassASTNodes(ast: Option[AST]): List[ClassDeclaration] = {
    ast match {
      case Some(ast) =>
        ast match {
          case ast: ClassDeclaration =>
            val classes = getClassASTNodes(ast.rightSibling)
            ast :: classes
          case _ =>
            val nodes = ast.rightSibling :: ast.children.map(c => Option(c))
            nodes.foldLeft[List[ClassDeclaration]](Nil) {
              (prevClasses, ast) =>
                val nextClasses  = getClassASTNodes(ast)
                prevClasses ++ nextClasses
            }
        }
      case None => Nil
    }
  }
}*/

object Package {
  def splitName(name: String): Array[String] = {
    name.split("\\.")
  }

  def nthParentName(split: Array[String], n: Int = 1): String = {
    split.slice(0, split.length - n).mkString(".")
  }

  def parentName(name: String): String = {
    nthParentName(splitName(name))
  }

  def parentNames(name: String): List[String] = {
    val split = splitName(name)
    val len = split.length - 1
    (1 to len).map(i => nthParentName(split, i)).toList
  }
}

class Package(val parent: Root, val ast: PackageDeclaration) extends Env {
  type ClassMap = Map[String, Class]
  var classes: ClassMap =
    AST.foldDown[ClassDeclaration, ClassMap](
      (ast, accMap) => {
        val cls = new Class(this, ast)
        accMap + (cls.name -> cls)
      },
      Some(ast),
      Map(),
      AST.RecursionOptions(true, true, true, (m1, m2) => m1 ++ m2)
    )
  /*
  var classes: Map[String, Class] = AST.fold[Map[String, Class]](
    (ast, accMap) => {
      ast match {
        case ast: ClassDeclaration =>
          val cls = new Class(this, ast)
          accMap + (cls.name -> cls)
        case _ => accMap
      }
    },
    Some(ast),
    Map(),
    AST.RecursionOptions(true, false, true, (m1, m2) => m1 ++ m2)
  )
  astClasses.foldLeft[Map[String, Class]]{
    (m, cn) => m + (cn.name -> Class.fromAST(this, cn))
  })
   */

  def name: String = {
    ast.name
  }

  def parentNames: List[String] = {
    Package.parentNames(name)
  }

  def addClass(cls: Class): Package = {
    classes = classes + (cls.name -> cls)
    this
  }

  def numClasses: Int = {
    classes.size
  }

  def hasClass(name: String): Boolean = {
    classes.contains(name)
  }

  def getClass(name: String): Option[Class] = {
    if (hasClass(name)) Some(classes(name)) else None
  }

  def getAllClasses: List[Class] = {
    classes.foldLeft(Nil: List[Class]) {
      case (acc, (_, cls)) =>
        cls :: acc
    }
  }

  def +(other: Package): Package = {
    if (other.name != name) {
      throw new RuntimeException()
    }

    classes = classes ++ other.classes
    this
  }

  /*
  def fromAST(ast: Option[AST]): Package = {
    ast match {
      case Some(ast) =>
        ast match {
          case ast: ClassDeclaration =>
            val pkg = fromAST(ast.rightSibling)
            val cls = Class.fromAST(this, ast)
            pkg.addClass(cls)
          case _ =>
            val nodes = ast.rightSibling :: ast.children.map(c => Option(c))
            val results = nodes.foldLeft[Package](this) {
              (prev, ast) =>
                val pkg = fromAST(ast)
                prev + pkg
            }
            results
        }
      case None => this
    }
  }
   */

  override def globalLookup(qualifiedName: String): Option[Env] = {
    // rootEnv.globalLookup(qualifiedName)
    None
  }
}
