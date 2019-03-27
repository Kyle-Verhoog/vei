package compiler.joos1w.asm

object ASM {
  def apply(rawCode: String): ASM = {
    new ASM(rawCode)
  }
}

class ASM(val rawCode: String, clean: Boolean = false) {
  def instructions: List[String] = {
    if (!clean) {
      val lines = rawCode.stripMargin.trim.split("\n").toList
      lines.map(l => l.trim.stripMargin)
    } else {
      rawCode.split("\n").toList
    }
  }

  def code: String = {
    instructions.mkString("\n") ++ "\n"
  }

  def ++(otherASM: ASM): ASM = {
    new ASM(code ++ otherASM.code)
  }

  // append code with an indent
  def +++(otherASM: ASM): ASM = {
    val otherCodef =
      otherASM.instructions.map(instr => "  " ++ instr).mkString("\n")
    new ASM(code ++ otherCodef ++ "\n", clean = true)
  }
}

class ProcedureCallASM(rawCode: String, procName: String) extends ASM(rawCode) {}
