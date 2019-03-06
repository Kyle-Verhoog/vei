package compiler.joos1w.env

import scala.reflect.ClassTag

final case class NameError(
    private val message: String = "Name error",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Name {
  def apply(name: String): Name = {
    new Name(name)
  }
}

class Name(val name: String) {
  val qualified: Boolean = isQualified(name)

  def isQualified(name: String): Boolean = {
    // TODO: determine exact defn
    // name.contains(".")
    // eg. classes defined without a package can be "qualified"
    // package A, A is qualified
    true
  }

  def qualifiedName: String = {
    if (qualified) {
      name
    } else {
      throw NameError(s"Attempt to get qualified name for unqualified $name")
    }
  }

  def toQualifiedName: QualifiedName = {
    if (qualified) {
      new QualifiedName(name)
    } else {
      throw NameError("TODO")
    }
  }

  override def toString: String = {
    val shortClass = getClass.toString.split("\\.").last
    s"$shortClass($name)"
  }

  def equals(other: ClassTag[Name]): Boolean = {
    other match {
      case other: Name => other.name == name
      case _           => false
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Name => name == that.name
      case _          => false
    }
  }

  override def hashCode(): Int = {
    name.hashCode
  }
}

class QualifiedName(override val qualifiedName: String)
    extends Name(qualifiedName) {
  lazy protected val split: Array[String] = mkSplit
  lazy val parentPackageNames: List[PackageName] = mkParentPackageNames
  lazy val parentName: PackageName = getParentPackageName

  def getParentPackageName: PackageName = {
    if (qualifiedName == "") {
      PackageName.ROOT
    } else {
      parentPackageNames.lastOption match {
        case Some(pkg) => pkg
        case None      => PackageName.ROOT
      }
    }
  }

  def mkSplit: Array[String] = {
    qualifiedName.split("\\.")
  }

  def mkNthParentPackageName(n: Int = 1): PackageName = {
    new PackageName(split.slice(0, split.length - n).mkString("."))
  }

  def mkParentPackageNames: List[PackageName] = {
    if (split.length > 1) {
      val len = split.length - 1
      (1 to len).map(i => mkNthParentPackageName(i)).toList
    } else {
      Nil
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: QualifiedName => that.qualifiedName == qualifiedName
      case that: Name          => throw NameError("comparing Name and QualifiedName")
      case _                   => false
    }
  }
}

object PackageName {
  val ROOT = PackageName("")

  def apply(name: String): PackageName = {
    new PackageName(name)
  }
}

class PackageName(override val qualifiedName: String)
    extends QualifiedName(qualifiedName) {
  val packageName: Name = mkPackageName

  def mkPackageName: Name = {
    Name(if (qualifiedName.contains(".")) {
      split.last
    } else {
      qualifiedName
    })
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: PackageName => that.qualifiedName == qualifiedName
      case _                 => false
    }
  }
}

class PackageItemName(pkgName: PackageName, itemName: String)
    extends QualifiedName(
      s"${pkgName.qualifiedName}${if (pkgName.qualifiedName.nonEmpty) "."
      else ""}$itemName") {
  val packageName: PackageName = getParentPackageName

  override def getParentPackageName: PackageName = {
    if (parentPackageNames.nonEmpty) parentPackageNames.last
    else PackageName.ROOT
  }
}

object ClassName {
  def apply(pkgName: PackageName, clsName: String): ClassName = {
    new ClassName(pkgName, clsName)
  }

}

class ClassName(pkgName: PackageName, clsName: String)
    extends QualifiedName(
      s"${pkgName.qualifiedName}${if (pkgName.qualifiedName.nonEmpty) "."
      else ""}$clsName") {
  val className: Name = Name(clsName)

  override def equals(that: Any): Boolean = {
    that match {
      case that: ClassName => that.qualifiedName == qualifiedName
      case _               => false
    }
  }
}

object InterfaceName {
  def apply(pkgName: PackageName, intName: String): InterfaceName = {
    new InterfaceName(pkgName, intName)
  }
}

class InterfaceName(pkgName: PackageName, intName: String)
    extends QualifiedName(
      s"${pkgName.qualifiedName}${if (pkgName.qualifiedName.nonEmpty) "."
      else ""}$intName") {
  val interfaceName: Name = Name(intName)

  override def equals(that: Any): Boolean = {
    that match {
      case that: InterfaceName => that.qualifiedName == qualifiedName
      case _                   => false
    }
  }
}
