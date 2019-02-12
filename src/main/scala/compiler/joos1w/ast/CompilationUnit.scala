package compiler.joos1w.ast

class CompilationUnit(name: String) extends AST {
  def fileName: String = {
    name.split("/").last.split("\\.java$").head.mkString
  }

  override def strFields: String = {
    s"$fileName.java"
  }
}
