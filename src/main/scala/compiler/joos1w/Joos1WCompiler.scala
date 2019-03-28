package compiler.joos1w

import ast.{AST, Weeder}
import java.io._
import java.nio._

object Joos1WCompiler {
  Joos1WScanner.loadSavedScanner()

  def compileFile(file: String): AST = {
    //println(s"Compiling file: $file")
    //println("Scanning...")
    val tokens = Joos1WScanner.scanFile(file)
    //println("Parsing...")
    val parseTree = Joos1WParser.parse(tokens, file)
    //println("Generating AST...")
    val ast = AST.fromParseTree(parseTree)
    //println("Weeding...")
    Weeder.weed(ast)
    ast
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def compileFiles(files: List[String]): Unit = {
    println("Scanning, parsing, weeding files...")
    val weededAsts =
      (files :+ "src/main/resources/AbstractKevin.java").map(file =>
        compileFile(file))

    weededAsts.foreach(ast => {
      Joos1WIfStatement.fixIfs(ast)
    })
    println("Building environments...")
    Joos1WEnvironment.buildEnvironment(weededAsts)
    println("Building environments...")
    //println("Other tree...")
    //val root = new Root().populateNamespace(weededAsts)
    //println(root.toStrTree)
    //Joos1WReachability.checkReachability(weededAsts)
    val asm = ASMFile(
      "runtime.s",
      """
        |
        | section .text
        | 
        | ; Allocates eax bytes of memory. Pointer to allocated memory returned in eax.
        |     global __malloc
        | __malloc:
        |     push eax
        |     mov eax, 45  ; sys_brk system call
        |     mov ebx, 0   ; 0 bytes - query current brk
        |     int 0x80
        |     pop ebx
        |     push eax
        |     add ebx, eax ; move brk ahead by number of bytes requested
        |     mov eax, 45  ; sys_brk system call
        |     int 0x80
        |     pop ebx
        |     cmp eax, 0   ; on error, exit with code 22
        |     jne ok
        |     mov eax, 22
        |     call __debexit
        | ok:
        |     mov eax, ebx
        |     ret
        | 
        | ; Debugging exit: ends the process, returning the value of
        | ; eax as the exit code.
        |     global __debexit
        | __debexit:
        |     mov ebx, eax
        |     mov eax, 1   ; sys_exit system call
        |     int 0x80
        | 
        | ; Exceptional exit: ends the process with exit code 13.
        | ; Call this in cases where the Joos code would throw an exception.
        |     global __exception
        | __exception:
        |     mov eax, 1   ; sys_exit system call
        |     mov ebx, 13
        |     int 0x80
        | 
        | ; Implementation of java.io.OutputStream.nativeWrite method.
        | ; Outputs the low-order byte of eax to standard output.
        |     global NATIVEjava.io.OutputStream.nativeWrite
        | NATIVEjava.io.OutputStream.nativeWrite:
        |     mov [char], al ; save the low order byte in memory
        |     mov eax, 4     ; sys_write system call
        |     mov ecx, char  ; address of bytes to write
        |     mov ebx, 1     ; stdout
        |     mov edx, 1     ; number of bytes to write
        |     int 0x80
        |     mov eax, 0     ; return 0
        |     ret
        | 
        | section .data
        | 
        | char:
        |     dd 0
      """.stripMargin
    ) :: Joos1WCodeGen.genCode(weededAsts)

    val pwd = System.getenv("PWD")
    println(s"OUTPUTTING TO $pwd")
    val outputDir = pwd ++ "/output"
    asm.foreach(code => {
      println(s"OUTPUTTING CODE ${code.fileName}")
      val fileName = outputDir ++ "/" ++ code.fileName
      printToFile(new File(fileName)) { p =>
        p.print(code.src)
      }
    })

    println("done")
  }
}
