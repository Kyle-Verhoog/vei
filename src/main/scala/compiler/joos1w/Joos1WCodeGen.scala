package compiler.joos1w

import asm._
import ast._
import compiler.joos1w.environment.{
  GenericEnvironment,
  ClassEnvironment,
  MethodEnvironment,
  VariableEnvironment,
  RootEnvironment,
}

case class ASMFile(fileName: String, src: String)

object Joos1WCodeGen {
  def javaToASMFileName(name: String): String = {
    name
      .split("\\.java$")
      .head
      .replace("/", "_") ++ ".s"
  }

  def fileName(ast: Option[AST]): String = {
    ast match {
      case Some(ast: CompilationUnit) =>
        javaToASMFileName(ast.rawFileName)
      case Some(ast: AST) =>
        fileName(ast.leftChild) ++ fileName(ast.rightSibling)
      case None => ""
    }
  }

  def methodDefinitionLabel(env: MethodEnvironment): String = {
    val pkgName = env
      .asInstanceOf[MethodEnvironment]
      .findEnclosingClass()
      .packageName
      .replaceAllLiterally(".", "_")
    val clsName = env
      .findEnclosingClass()
      .myAst
      .asInstanceOf[ClassDeclaration]
      .identifier

    val id = env.myAst.asInstanceOf[ASTMethodDeclaration].identifier
    val params = env.myAst match {
      case method: MethodDeclaration =>
        method.header match {
          case Some(header: MethodHeader) =>
            header.methodDeclarator.parameters.children
              .map(p => p.asInstanceOf[FormalParameter].ttype)
              .mkString("_")
          case _ => ""
        }
      case method: ConstructorDeclaration =>
        method.constructorDeclarator.parameters.children
          .map(p => p.asInstanceOf[FormalParameter].ttype)
          .mkString("")
      case _ => ""
    }
    val methName =
      s"${id}_$params" //env.myAst.toString // ++ meth.toString.hashCode.toString
        .replaceAllLiterally(".", "_")
        .replaceAllLiterally("(", "_")
        .replaceAllLiterally(")", "_")
        .replaceAllLiterally(" ", "_")
        .replaceAllLiterally("[", "_")
        .replaceAllLiterally("]", "_")
    s"${pkgName}_${clsName}_$methName"
  }

  def classLabel(env: ClassEnvironment): String = {
    val pkgName = env.packageName
      .replaceAllLiterally(".", "_")
    env.myAst match {
      case ast: ClassDeclaration =>
        val clsName = ast.identifier
        s"${pkgName}_$clsName"
      case ast: InterfaceDeclaration =>
        val clsName = ast.identifier
        s"${pkgName}_$clsName"
    }
  }

  def classDefinitionLabel(env: ClassEnvironment): String = {
    val clsLabel = classLabel(env)
    s"$clsLabel"
  }

  def classVTableLabel(env: ClassEnvironment): String = {
    val clsLabel = classLabel(env)
    s"VT_$clsLabel"
  }

  def staticFieldLabel(env: VariableEnvironment): String = {
    val pkgName = env
      .findEnclosingClass()
      .packageName
      .replaceAllLiterally(".", "_")
    val clsName = env
      .findEnclosingClass()
      .myAst
      .asInstanceOf[ClassDeclaration]
      .identifier

    val fieldType = env.myAst.asInstanceOf[FieldDeclaration].fieldType
    val fieldName = env.myAst.asInstanceOf[FieldDeclaration].name
    s"field_${pkgName}_${clsName}_${fieldType}_$fieldName"
  }

  def classASM(cls: ClassDeclaration): ASM = {
    val clsEnv = cls.env.asInstanceOf[ClassEnvironment]
    val clsLabel = classLabel(clsEnv)
    val clsVTableLabel = classVTableLabel(clsEnv)

    var fields = List[VariableEnvironment]()
    clsEnv.containSet.values.foreach({
      case fieldEnv: VariableEnvironment =>
        if (fieldEnv.modifiers.contains("static")) {
          fields = fieldEnv :: fields
        }
      case _ =>
    })

    // sort the fields by their order
    fields = fields.sortBy(f => f.order)
    var initCode = ASM(s"""
                          |cls_init_$clsLabel:
       """.stripMargin)
    fields.foreach(f => {
      val fieldCode =
        astASM(Some(f.myAst.asInstanceOf[FieldDeclaration].variableDeclarator))
      val fieldLabel = staticFieldLabel(f)
      initCode = initCode ++ ASM(";; Initializing field")
      initCode = initCode ++
        fieldCode ++
        new ASM(
          s"""
           | mov ebx, $fieldLabel
           | mov [ebx], eax
         """.stripMargin,
          data = s"""
               |$fieldLabel:
               |dd 0
             """.stripMargin
        )
    })
    initCode = initCode ++ ASM("""
        | ret ;; end of class initialization procedure
      """.stripMargin)

    ASM(s"""
           |extern __malloc
           |extern __exception
           |extern $clsVTableLabel
           |
           |;; class initializer
           |global cls_init_$clsLabel
           |global $clsLabel
           |$clsLabel:
           |dd $clsVTableLabel
           |""".stripMargin) ++
      astASM(Some(cls.getClassBody)) ++
      initCode
  }

  def interfaceASM(int: InterfaceDeclaration): ASM = {
    ASM(";; TODO interfaces")
  }

  def astASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(ast: CompilationUnit) =>
        astASM(ast.typeDeclaration)
      case Some(ast: TypeDeclaration) =>
        astASM(ast.leftChild)
      case Some(cls: ClassDeclaration) =>
        classASM(cls)
      case Some(interfaceDeclaration: InterfaceDeclaration) =>
        interfaceASM(interfaceDeclaration)
      case Some(meth: ConstructorDeclaration) =>
        // TODO constructors
        val env = meth.env.asInstanceOf[MethodEnvironment]
        val methDefLabel = methodDefinitionLabel(env)
        // TODO: arrays cannot depend on varCount, need frameSize or similar
        val frameSize = env.varCount * 4
        ASM(s"""global $methDefLabel
                 |$methDefLabel:
                 |push ebp  ;; push stack frame pointer
                 |mov ebp, esp
                 |sub esp, $frameSize ;; push the stack frame""".stripMargin) ++
          MethodASM.methodASM(Some(meth.body)) ++
          ASM(s"""
                   |.method_end:
                   |add esp, $frameSize ;; pop the frame
                   |pop ebp   ;; pop stack frame pointer
                   |ret
                   |""".stripMargin)
      case Some(meth: MethodDeclaration) =>
        val env = meth.env.asInstanceOf[MethodEnvironment]
        val methDefLabel = methodDefinitionLabel(env)
        // TODO: arrays cannot depend on varCount, need frameSize or similar
        val frameSize = env.varCount * 4
        ASM(s"""global $methDefLabel
               |$methDefLabel:
               |push ebp  ;; push stack frame pointer
               |mov ebp, esp
               |sub esp, $frameSize ;; push the stack frame""".stripMargin) ++
          MethodASM.methodASM(Some(meth.body)) ++
          ASM(s"""
               |.method_end:
               |add esp, $frameSize ;; pop the frame
               |pop ebp   ;; pop stack frame pointer
               |ret
               |""".stripMargin)
      case Some(fieldDeclaration: FieldDeclaration) =>
        // class fields are handled in classASM
        ASM("")
      // if (fieldDeclaration.modifiers.contains("static")) {
      //   val label = staticFieldLabel(
      //     fieldDeclaration.env.asInstanceOf[VariableEnvironment])
      //   new ASM(
      //     text = s"""""".stripMargin,
      //     data = s"""
      //        |;; Static field location ${fieldDeclaration.name}
      //        |$label:
      //        |dd 0
      //      """.stripMargin
      //   )
      // } else {
      //   ASM(s"""
      //          |;; Field declaration
      //    """.stripMargin)
      // }
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astASM(Some(ast)))
          case s =>
            throw new MatchError(
              s"astASM match error on ASTList $s ${astList.parent.get.parent.get.toStrTree}")
        }
      case Some(name: Name) =>
        // resolve a name when initializing fields
        NameASM.nameASM(Some(name))
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH astASM on $ast")
        CommonASM.commonASM(Some(ast), astASM)
      case _ => throw new MatchError(s"methodAST match error on $ast")
    }
  }

  def astStaticIntTestASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(ast: CompilationUnit) =>
        astStaticIntTestASM(ast.typeDeclaration)
      case Some(ast: TypeDeclaration) =>
        astStaticIntTestASM(ast.leftChild)
      case Some(cls: ClassDeclaration) =>
        astStaticIntTestASM(Some(cls.getClassBody))
      case Some(meth: MethodDeclaration) =>
        if (meth.identifier == "test") {
          val env = meth.env.asInstanceOf[MethodEnvironment]
          val methDefLabel = methodDefinitionLabel(env)
          ASM(s"""
                 |extern $methDefLabel
                 |call $methDefLabel
                 |""".stripMargin)
        } else {
          ASM("")
        }
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astStaticIntTestASM(Some(ast)))
        }
      case _ => ASM(s";; TODO $ast") // TODO
    }
  }

  def astVTableASM(classes: List[ClassEnvironment],
                   methods: List[MethodEnvironment]): ASM = {
    var asm = ASM("")
    classes.foreach(cls => {
      val clsVTLabel = classVTableLabel(cls)
      var methCount = 0
      asm = asm ++ new ASM("",
                           data = s"""
           |global $clsVTLabel
           |$clsVTLabel:
           |""".stripMargin)
      methods.foreach(method => {
        method.vtOffset = Some(methCount)
        methCount = methCount + 1
        val methodLabel = methodDefinitionLabel(method)
        asm = asm ++ new ASM(
          text = "",
          data = s"""
               |extern $methodLabel
               |dd $methodLabel
             """.stripMargin
        )
      })
    })
    asm
  }

  // this method is called with the AST of the class containing the
  // public static int test() method.
  // find the class name and use it to invoke the test method
  def astMainASM(ast: Option[AST], classes: List[ClassEnvironment]): ASM = {
    var methods = List[MethodEnvironment]()
    classes.foreach(clsEnv => {
      methods = methods ++ clsEnv.methodTable.values
    })
    val classInitCode = classes.foldLeft(ASM(""))((acc, cls) => {
      cls.myAst match {
        case c: ClassDeclaration =>
          val clsLabel = classLabel(cls)
          acc ++ ASM(s"""
                        |extern cls_init_$clsLabel
                        |call cls_init_$clsLabel
         """.stripMargin)
        case i: InterfaceDeclaration =>
          // TODO: do interfaces have any initialization?
          acc
        case _ => acc
      }
    })
    val vtableASM = astVTableASM(classes, methods)
    val staticIntTestCode = astStaticIntTestASM(ast)
    ASM(s"""
      | global _start
      |_start:""") ++
      classInitCode ++
      staticIntTestCode ++
      ASM(s"""
      |mov ebx, eax
      |mov eax, 1
      |int 0x80""") ++ vtableASM
  }

  def mkMainASMFile(ast: AST, classes: List[ClassEnvironment]): ASMFile = {
    ASMFile("__main.s", astMainASM(Some(ast), classes)._code)
  }

  def mkASMFile(ast: AST): ASMFile = {
    ASMFile(fileName(Some(ast)), astASM(Some(ast))._code)
  }

  def genCode(asts: List[AST], rootEnv: GenericEnvironment): List[ASMFile] = {
    var classes = List[ClassEnvironment]()
    rootEnv
      .asInstanceOf[RootEnvironment]
      .packageEnvironments
      .values
      .foreach(pkgEnv => {
        classes = classes ++ pkgEnv.classTable.values
      })
    mkMainASMFile(asts.head, classes) :: asts.map(ast => mkASMFile(ast))
  }
}
