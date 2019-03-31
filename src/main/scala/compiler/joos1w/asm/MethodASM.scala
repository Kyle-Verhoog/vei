package compiler.joos1w.asm

import compiler.joos1w.asm.CommonASM.incrementAndReturnCounter
import compiler.joos1w.ast._
import compiler.joos1w.environment._
import compiler.joos1w.environment.types.{CustomType, StringType}

object MethodASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def methodASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(methodBody: MethodBody) =>
        methodASM(methodBody.leftChild)
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(v: LocalVariableDeclaration) =>
        val env = v.env.asInstanceOf[VariableEnvironment]
        val offset = env.offset * 4
        val declCode = methodASM(Some(v.variableDeclarator))
        ASM(s";; ${v.ttype} ${v.name} = ${v.variableDeclarator}") ++
          declCode ++
          ASM(s"mov [ebp - $offset], eax ;; assign variable ${v.name}")
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
        val myCounter = incrementAndReturnCounter
        val lhsCode = methodASM(Some(assignment.getLHS))
        val rhsCode = methodASM(Some(assignment.getRHS))

        val subTypeCheckCode =
          assignment.getLHS match {
            // if LHS is array access, do subtype check
            case lhs: ArrayAccess =>
              val rhsType = environment.determineType(assignment.getRHS,
                                                      assignment.getRHS.env)

              // if RHS is not a primitive, we do the subtype check
              rhsType match {
                case ttype @ ((_: StringType) | (_: CustomType)) =>
                  ASM(s"""
                       |;; perform array access sub type check
                       |extern __exception
                       |mov ecx, [ebx + 4] ;; get addr to subclass table for rhs
                       |pop edx ;; get pointer to lhs WE MUST PUT THIS BACK ON THE STACK
                       |mov edx, [edx + 8] ;; get offset of subclass table for lhs
                       |push edx ;; put back lhs pointer
                       |push eax ;; save rhs value
                       |cmp 0xffffffff, [ecx + edx] ;; check if rhs is subclass of lhs
                       |je .array_subclass_check_pass${myCounter}
                       |call __exception
                       |.array_subclass_check_pass${myCounter}:
                       |pop eax ;; restore rhs value, finished array access sub type check
           """.stripMargin)
                case _ => ASM(s"""""")
              }
            case _ => ASM(s"""""")
          }

        ASM(s";; ${assignment.getLHS} := ${assignment.getRHS}") ++
          ASM(s";; := eval lhs") ++
          lhsCode ++
          ASM(s";; := saving lhs") ++
          ASM(s"push ebx") ++
          ASM(s";; := eval rhs") ++
          rhsCode ++
          ASM(s""";; := eax has rhs, ebx has rhs pointer""") ++
          subTypeCheckCode ++
          ASM(s";; lhs := rhs") ++
          ASM(s"""
             |mov ebx, eax
             |pop eax
             |mov [eax], ebx
           """.stripMargin)
      case Some(thisCall: ThisCall) =>
        ASM(s"mov eax, [ebp] ;; this reference")
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
