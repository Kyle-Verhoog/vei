package compiler.joos1w.asm

object ASM {
  def apply(rawCode: String): ASM = {
    new ASM(rawCode)
  }
}

class ASM(val rawCode: String) {
  def code: String = {
    rawCode
  }

  def ++(otherCode: ASM): ASM = {
    new ASM(rawCode ++ otherCode.code)
  }
}

class ProcedureCallASM(rawCode: String, procName: String) extends ASM(rawCode) {
}

