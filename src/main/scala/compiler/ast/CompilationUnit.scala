package compiler.ast

class CompilationUnit(name: String) extends AST {
  def fileName: String = {
    name.split("/").last.split("\\.").head
  }
}
