package compiler.joos1w

import asm._
import ast._
import compiler.joos1w.environment._

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
    val clsSubClsLabel = classSubClassTableLabel(clsEnv)

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
        astASM(Some(f.myAst.asInstanceOf[FieldDeclaration].variableDeclarator),
               false)
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
               |global $fieldLabel
               |$fieldLabel:
               |dd 0
             """.stripMargin
        )
    })
    initCode = initCode ++ ASM("""
        | ret ;; end of class initialization procedure
      """.stripMargin)

    ASM(s"""
           |
           |;; class initializer
           |global cls_init_$clsLabel
           |global $clsLabel
           |$clsLabel:
           |dd $clsVTableLabel
           |dd $clsSubClsLabel
           |dd ${4 * clsEnv.subClsTableOffset}
           |""".stripMargin) ++
      astASM(Some(cls.getClassBody), false) ++
      initCode
  }

  def interfaceASM(int: InterfaceDeclaration): ASM = {
    ASM(";; TODO interfaces")
  }

  def paramOffset(env: MethodEnvironment): Int = {
    4 * (env.nparams + 1)
  }

  // def varOffset(env: VariableEnvironment): Int = {
  //   val methodEnv = env.findEnclosingMethod()
  //   val paramOffset = paramOffset(methodEnv)
  //   val frameOffset = 4 * env.offset
  //   paramOffset + frameOffset
  // }

  def methodASM(env: MethodEnvironment, bodyASM: ASM): ASM = {
    val methDefLabel = methodDefinitionLabel(env)
    val frameSize = 4 * env.localVarCount
    if (methDefLabel == "java_io_OutputStream_nativeWrite_int") {
      new ASM(
        s"""
           |; Implementation of java.io.OutputStream.nativeWrite method.
           |; Outputs the low-order byte of eax to standard output.
           |global ${methDefLabel}
           |${methDefLabel}:
           |    mov [char], al ; save the low order byte in memory
           |    mov eax, 4     ; sys_write system call
           |    mov ecx, char  ; address of bytes to write
           |    mov ebx, 1     ; stdout
           |    mov edx, 1     ; number of bytes to write
           |    int 0x80
           |    mov eax, 0     ; return 0
           |    ret
         """.stripMargin,
        data = s"""
                  | char:
                  |     dd 0
         """.stripMargin
      )
    } else {
      ASM(s"""global $methDefLabel
           |$methDefLabel:
           |push ebp
           |mov ebp, esp ;; frame pointer <- stack pointer
           |sub esp, $frameSize ;; push the stack frame""".stripMargin) ++
        bodyASM ++
        ASM(s"""|.method_end:
              |mov esp, ebp ;; reset stack pointer
              |pop ebp
              |ret
              |""".stripMargin)
    }
  }

  def objRefOffset(methodEnv: MethodEnvironment): ASM = {
    val objRefOffset = 4 * methodEnv.paramCount
    ASM(s"""
           | mov eax, [ebp + $objRefOffset] ;; reference to obj
          """.stripMargin)
  }

  def astASM(ast: Option[AST], lvalue: Boolean): ASM = {
    ast match {
      case Some(ast: CompilationUnit) =>
        astASM(ast.typeDeclaration, lvalue)
      case Some(ast: TypeDeclaration) =>
        astASM(ast.leftChild, lvalue)
      case Some(cls: ClassDeclaration) =>
        classASM(cls)
      case Some(interfaceDeclaration: InterfaceDeclaration) =>
        interfaceASM(interfaceDeclaration)
      case Some(const: ConstructorDeclaration) =>
        val env = const.env.asInstanceOf[MethodEnvironment]
        val objRefOffset = 4 * env.paramCount
        val bodyASM = MethodASM.methodASM(Some(const.children(1)),
                                          lvalue = false) ++
          ASM(s"""
            | mov eax, [ebp + $objRefOffset] ;; constructor returns reference to obj
          """.stripMargin)
        val nparams = const.rawParameters.length
        env.nparams = nparams // no +1 because constructors have another arg added
        ASM(s";; constructor definition") ++
          methodASM(env, bodyASM) ++ ASM(s";; constructor end")
      case Some(meth: MethodDeclaration) =>
        val env = meth.env.asInstanceOf[MethodEnvironment]
        val bodyASM = MethodASM.methodASM(Some(meth.body), lvalue = false)
        val nparams = meth.header.get.parameters.length
        env.nparams = nparams // + 1 for implicit this reference
        ASM(s";; method definition") ++
          methodASM(env, bodyASM) ++ ASM(s";; method end")
      case Some(fieldDeclaration: FieldDeclaration) =>
        // class fields are handled in classASM
        ASM("")
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astASM(Some(ast), lvalue))
          case s =>
            throw new MatchError(
              s"astASM match error on ASTList $s ${astList.parent.get.parent.get.toStrTree}")
        }
      case Some(name: Name) =>
        // resolve a name when initializing fields
        NameASM.nameASM(Some(name), lvalue = false)
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH astASM on $ast")
        CommonASM.commonASM(Some(ast), astASM, lvalue = false)
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
                 |mov eax, 0 ;; push null "this" reference
                 |push eax
                 |call $methDefLabel
                 |pop ebx ;; pop null "this" reference
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
          acc ++
            ASM(s"""
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

  def classSubClassTableLabel(clsEnv: ClassEnvironment): String = {
    val clsLabel = classLabel(clsEnv)
    s"sub_classes_$clsLabel"
  }

  def subclass1IsSubclassOfClass2Label(clsEnv1: ClassEnvironment,
                                       clsEnv2: ClassEnvironment): String = {
    val clsLabel1 = classLabel(clsEnv1)
    val clsLabel2 = classLabel(clsEnv2)
    s"is_subclass__${clsLabel1}__of__$clsLabel2"
  }

  def subClassTable(classes: List[ClassEnvironment]): ASM = {
    var subClsTable = ASM("")
    classes.foreach(cls1 => {
      val clsLabel = classSubClassTableLabel(cls1)
      subClsTable = subClsTable ++ ASM(s"""
          | global $clsLabel ;; ${cls1.myAst} subclass entry
          | $clsLabel:
        """.stripMargin)
      var i = 0
      classes.foreach(cls2 => {
        val label = subclass1IsSubclassOfClass2Label(cls1, cls2)
        val isSubCls = cls1.isSubClassOf(cls2)
        val res = if (isSubCls) "0xffffffff" else "0x0"
        cls2.subClsTableOffset = i
        i += 1
        subClsTable = subClsTable ++ ASM(s"""
            |global $label
            |$label:
            |dd $res  ;; ${cls1.myAst} is${if (!isSubCls) " not" else ""} a subclass of ${cls2.myAst}
          """.stripMargin)
      })
    })
    subClsTable
  }

  def mkMainASMFile(ast: AST, classes: List[ClassEnvironment]): ASMFile = {
    ASMFile("__main.s", astMainASM(Some(ast), classes)._code)
  }

  def mkASMFile(ast: AST): ASMFile = {
    ASMFile(fileName(Some(ast)), astASM(Some(ast), lvalue = false)._code)
  }

  def mkSubClassASMFile(classes: List[ClassEnvironment]): ASMFile = {
    ASMFile("__subclassTable.s", subClassTable(classes)._code)
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
    mkMainASMFile(asts.head, classes) ::
      mkSubClassASMFile(classes) :: asts.map(ast => mkASMFile(ast))
    // mkMainASMFile(asts.head, classes) :: asts.map(ast => mkASMFile(ast))
  }

  def resolveQualifiedName(
      qualifiedName: String,
      enclosingEnv: GenericEnvironment): List[GenericEnvironment] = {
    var i = 0
    val partsOfName = qualifiedName.split('.')
    var finalParts = List[GenericEnvironment]()

    // resolve all parts of the name
    while (i < partsOfName.length) {
      val possibleName = resolveQualifiedNameHead(
        partsOfName.take(i + 1).mkString("."),
        enclosingEnv)
      if (possibleName.isDefined) { // this means we found the first part of the name
        finalParts = finalParts :+ possibleName.get

        i += 1
        while (i < partsOfName.length) { // resolve remaining parts of the name
          finalParts = finalParts :+ resolveQualifiedNamePart(partsOfName(i),
                                                              finalParts.last)
          i += 1
        }
      }
      i += 1
    }

    finalParts
  }

  // will return either a variable env, class env or None,
  def resolveQualifiedNameHead(
      head: String,
      enclosingEnv: GenericEnvironment): Option[GenericEnvironment] = {
    if (enclosingEnv.serarchForVariable(head).isDefined) {
      enclosingEnv.serarchForVariable(head)
    } else if (enclosingEnv.lookupClass(head).isDefined) {
      enclosingEnv.lookupClass(head)
    } else {
      None
    }
  }

  def resolveQualifiedNamePart(
      name: String,
      previousEnv: GenericEnvironment): VariableEnvironment = {
    try {
      previousEnv
        .findEnclosingClass()
        .containSet
        .get(name, None)
        .get
        .asInstanceOf[VariableEnvironment]
    } catch {
      case e: Exception =>
        if (name == "length") {
          new LengthEnvironmentt()
        } else {
          throw e
        }
    }
  }

  def stringToChar(str: String): Char = {
    str match {
      case "'\\''"  => '\''
      case "'\\\"'" => '\"'
      case "'\\\'"  => '\\'
      case "'\\n'"  => '\n'
      case "'\\r'"  => '\r'
      case "'\\t'"  => '\t'
      case "'\\b'"  => '\b'
      case "'\\f'"  => '\f'
      case "'\\0'"  => '\0'
      case _ => {
        if (str.length != 3) {
          throw new RuntimeException(
            s"Do not know how to convert string:    ${str}    to char")
        } else {
          str(1)
        }
      }
    }
  }
}
