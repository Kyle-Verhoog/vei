package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment._

object MethodASM {
  def methodASM(ast: Option[AST]): ASM = {
    println(ast)
    ast match {
      case Some(methodBody: MethodBody) =>
        if (methodBody.hasBody) methodASM(methodBody.leftChild) else ASM("")
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(v: LocalVariableDeclaration) =>
        val env = v.env.asInstanceOf[VariableEnvironment]
        val offset = env.offset.get * 4
        val declCode = methodASM(Some(v.variableDeclarator)).code
        ASM(s""";; ${v.ttype} ${v.name} = ${v.variableDeclarator}
               |$declCode
               |mov [ebp - $offset], eax
           """.stripMargin)
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "block_statements" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ methodASM(Some(ast)))
          case _ =>
            throw new MatchError(s"methodASM match error on ASTList $ast")
        }
      case Some(stmt: TopLevelIf) =>
        StatementASM.topLevelIfStatementASM(stmt)
      case Some(retAST: Return) =>
        val exprCode = methodASM(Some(retAST.expr())).code
        ASM(s""";; return <expr>
               |$exprCode jmp .method_end
               |""".stripMargin)
      case Some(forStatement: ForStatement) =>
        ASM(s";; TODO for statement")
      case Some(whileStatement: WhileStatement) =>
        ASM(s";; TODO while statement")
      case Some(assignment: Assignment) =>
        val lhsCode = methodASM(Some(assignment.getLHS)).code
        val rhsCode = methodASM(Some(assignment.getRHS)).code
        ASM(s""";; ${assignment.getLHS} := ${assignment.getRHS}
             |$lhsCode
             |push ebx
             |$rhsCode
             |mov ebx, eax
             |pop eax
             |mov [eax], ebx
           """.stripMargin)
      case Some(thisCall: ThisCall) =>
        ASM(s";; TODO this call")
      case Some(name: Name) =>
        name.env.findLocalVariable(name.name) match {
          case Some(v: VariableEnvironment) =>
            val offset = v.offset.get * 4
            ASM(s"""mov ebx, ebp - $offset ;; &var ${name.name}
                 |  mov eax, [ebp - $offset] ;; *var ${name.name}
               """.stripMargin)

          // check if it's a field
          case None =>
            ASM(";; TODO field/qualified name lookup")
          // name.env.serarchForVariable(name.name) match {
          //   case Some(v: VariableEnvironment) => ASM(";; TODO field lookup")
          //   case None                         => ASM(";; ERROR variable not found")
          // }
        }
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH methodASM on $ast")
        CommonASM.commonASM(Some(ast), MethodASM.methodASM)
      case _ => throw new MatchError(s"methodAST match error on $ast")
    }
  }
}
