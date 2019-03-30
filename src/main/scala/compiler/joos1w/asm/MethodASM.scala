package compiler.joos1w.asm

import compiler.joos1w.ast._
import compiler.joos1w.environment._

object MethodASM {
  def methodASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(methodBody: MethodBody) =>
        if (methodBody.hasBody) methodASM(methodBody.leftChild) else ASM("")
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(v: LocalVariableDeclaration) =>
        val env = v.env.asInstanceOf[VariableEnvironment]
        val offset = env.offset.get * 4
        val declCode = methodASM(Some(v.variableDeclarator))
        ASM(s";; ${v.ttype} ${v.name} = ${v.variableDeclarator}") ++
          declCode ++
          ASM(s"mov [ebp - $offset], eax")
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
        val exprCode = methodASM(Some(retAST.expr()))
        ASM(s";; return ${retAST.expr()}") ++
          exprCode ++
          ASM("jmp .method_end")
      case Some(forStatement: ForStatement) =>
        StatementASM.forStatementASM(forStatement)
      case Some(whileStatement: WhileStatement) =>
        StatementASM.whileStatementASM(whileStatement)
      case Some(assignment: Assignment) =>
        val lhsCode = methodASM(Some(assignment.getLHS))
        val rhsCode = methodASM(Some(assignment.getRHS))
        ASM(s";; ${assignment.getLHS} := ${assignment.getRHS}") ++
          ASM(s";; := eval lhs") ++
          lhsCode ++
          ASM(s";; := saving lhs") ++
          ASM(s"push ebx") ++
          ASM(s";; := eval rhs") ++
          rhsCode ++
          ASM(s";; lhs := rhs") ++
          ASM(s"""
             |mov ebx, eax
             |pop eax
             |mov [eax], ebx
           """.stripMargin)
      case Some(thisCall: ThisCall) =>
        ASM(s"mov eax, [ebp - 0] ;; this reference")
      case Some(name: Name) =>
        NameASM.nameASM(Some(name))
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH methodASM on $ast")
        CommonASM.commonASM(Some(ast), MethodASM.methodASM)
      case None => ASM("")
      case _    => throw new MatchError(s"methodASM match error on $ast")
    }
  }
}
