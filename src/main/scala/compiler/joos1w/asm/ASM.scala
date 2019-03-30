package compiler.joos1w.asm

object ASM {
  def apply(rawCode: String): ASM = {
    new ASM(rawCode)
  }
}

class ASM(val text: String,
          val data: String = "",
          val bss: String = "",
          isClean: Boolean = false) {
  def clean(code: String): List[String] = {
    if (!isClean) {
      val lines = code.stripMargin.trim.split("\n").toList
      lines.map(l => l.trim.stripMargin)
    } else {
      code.split("\n").toList
    }
  }

  def addData(data: String): ASM = {
    new ASM(
      text = text,
      data = data ++ data,
      bss = bss,
      isClean = isClean,
    )
  }

  def instructions: List[String] = {
    val cleanText = clean(text)
    val cleanData = clean(data)
    val cleanBSS = clean(bss)

    (if (cleanText.nonEmpty)
       List("\nsection .text") ++ cleanText
     else
       Nil) ++ (if (cleanData.nonEmpty)
                  List("\nsection .data") ++ cleanData
                else
                  Nil) ++ (if (cleanBSS.nonEmpty)
                             List("\nsection .bss") ++ cleanBSS
                           else Nil)
  }

  def _code: String = {
    instructions.mkString("\n") ++ "\n"
  }

  def ++(otherASM: ASM): ASM = {
    new ASM(
      text = text ++ (if (text.nonEmpty && otherASM.text.nonEmpty) "\n" else "") ++ otherASM.text,
      data = data ++ (if (data.nonEmpty && otherASM.data.nonEmpty) "\n" else "") ++ otherASM.data,
      bss = bss ++ (if (bss.nonEmpty && otherASM.bss.nonEmpty) "\n" else "") ++ otherASM.bss,
    )
  }

  // append code with an indent
  def +++(otherASM: ASM): ASM = {
    val otherCodef =
      otherASM.instructions.map(instr => "  " ++ instr).mkString("\n")
    new ASM(_code ++ otherCodef ++ "\n", isClean = true)
  }
}

class ProcedureCallASM(rawCode: String, procName: String) extends ASM(rawCode) {}
