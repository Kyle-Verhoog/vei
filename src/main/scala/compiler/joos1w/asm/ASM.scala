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

  def addExterns(code: String): String = {
    var externs = Map[String, String]()
    code
      .split("\n")
      .foreach(line => {
        var skip = false

        line
          .split(" ")
          .foreach(word => {
            if (word == ";;" || word == ";") {
              // skip the rest of the line
              skip = true
            } else {
              // if the code does not contain the definition for a label then add it
              // to the accumulator
              if (!skip && !externs.contains(word) &&
                  word.contains("_") &&
                  !word.contains(":") && !word.startsWith(".") && !code
                    .contains(word ++ ":")) {
                // val label = word.contains(":")
                //println(s"${word ++ ":"} wtf   ${!code.contains(word ++ ":")}")
                val externStmt = s"extern $word ;; import $word"
                externs = externs + (word -> externStmt)
              }
            }
          })
      })
    externs.values.mkString("\n") ++ "\n\n" ++ code
  }

  def _code: String = {
    val codeWithoutExterns = instructions.mkString("\n") ++ "\n"
    addExterns(codeWithoutExterns)
    // val codeWithoutExterns = instructions.mkString("\n") ++ "\n"
  }

  def ++(otherASM: ASM): ASM = {
    new ASM(
      text = text ++ (if (text.isEmpty || otherASM.text.isEmpty) "" else "\n") ++ otherASM.text,
      data = data ++ (if (data.isEmpty || otherASM.data.isEmpty) "" else "\n") ++ otherASM.data,
      bss = bss ++ (if (bss.isEmpty || otherASM.bss.isEmpty) "" else "\n") ++ otherASM.bss,
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
