package compiler.joos1w.env

class Primitive(rawName: String) extends Env {
  val name: QualifiedName = new QualifiedName(rawName)

  override def globalLookup(name: Name): Option[Env] = {
    None
  }

  override def lookup(name: Name): Option[Env] = {
    None
  }

  override def toStrTree: String = {
    s"$name"
  }
}
