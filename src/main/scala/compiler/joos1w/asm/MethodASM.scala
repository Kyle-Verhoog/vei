package compiler.joos1w.asm

import compiler.joos1w.asm.CommonASM.incrementAndReturnCounter
import compiler.joos1w.ast._
import compiler.joos1w.environment._
import compiler.joos1w.environment.types.{ArrayType, CustomType, StringType}

object MethodASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def methodASM(ast: Option[AST], lvalue: Boolean): ASM = {
    ast match {
      case Some(methodBody: MethodBody) =>
        methodASM(methodBody.leftChild, lvalue)
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(v: LocalVariableDeclaration) =>
        val env = v.env.asInstanceOf[VariableEnvironment]
        val offset = 4 * env.localVarOffset
        val declCode = methodASM(Some(v.variableDeclarator), lvalue)
        ASM(s";; ${v.ttype} ${v.name} = ${v.variableDeclarator}") ++
          declCode ++
          ASM(s"mov [ebp - $offset], eax ;; assign variable ${v.name}")
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "block_statements" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ methodASM(Some(ast), lvalue))
          case _ =>
            throw new MatchError(s"methodASM match error on ASTList $ast")
        }
      case Some(stmt: TopLevelIf) =>
        StatementASM.topLevelIfStatementASM(stmt)
      case Some(retAST: Return) =>
        val exprCode = methodASM(Some(retAST.expr()), lvalue)
        ASM(s";; return ${retAST.expr()}") ++
          exprCode ++
          ASM("jmp .method_end")
      case Some(forStatement: ForStatement) =>
        StatementASM.forStatementASM(forStatement)
      case Some(whileStatement: WhileStatement) =>
        StatementASM.whileStatementASM(whileStatement)
      case Some(assignment: Assignment) =>
        val myCounter = incrementAndReturnCounter
        val lhsCode = methodASM(Some(assignment.getLHS), lvalue = true)
        val rhsCode = methodASM(Some(assignment.getRHS), lvalue = false)

        val subTypeCheckCode =
          assignment.getLHS match {
            // if LHS is array access, do subtype check
            case lhs: ArrayAccess =>
              val rhsType = environment.determineType(assignment.getRHS,
                                                      assignment.getRHS.env)

              // if RHS is not a primitive, we do the subtype check
              // TODO handle primitive
              rhsType match {
                case ttype @ ((_: StringType) | (_: CustomType)) =>
                  ASM(s"""
                       |;; perform array access sub type check
                       |;; assume address to array is in eax
                       |;; mov ecx, [ebx]
                       |mov ecx, [eax + 4] ;; get addr to subclass table for rhs
                       |pop edx ;; get pointer to lhs WE MUST PUT THIS BACK ON THE STACK
                       |mov ebx, [edx]
                       |mov edx, [ebx + 8] ;; get offset of subclass table for lhs
                       |push edx ;; put back lhs pointer
                       |push eax ;; save rhs value
                       |mov eax, 0xffffffff
                       |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                       |je .array_subclass_check_pass${myCounter}
                       |call __exception
                       |.array_subclass_check_pass${myCounter}:
                       |pop eax ;; restore rhs value, finished array access sub type check
           """.stripMargin)
                case _ => ASM(s"""""")
              }
            // otherwise, check type of lhs, if it's an array we want to make sure it is assignable
            case _ =>
              if (environment
                    .determineType(assignment.getLHS, assignment.env)
                    .isInstanceOf[ArrayType]) {
                // get lhs arrays type
                val lhsType = environment
                  .determineType(assignment.getLHS, assignment.env)
                  .asInstanceOf[ArrayType]
                  .rootType
                val rhsType = environment.determineType(assignment.getRHS,
                                                        assignment.getRHS.env)
                // if RHS is not a primitive, we do the subtype check
                // TODO handle primitive
                rhsType match {
                  case ttype @ ((_: StringType) | (_: CustomType)) =>
                    ASM(s"""
                         |;; perform array access sub type check
                         |;; assume address to array is in eax
                         |;; mov ecx, [ebx]
                         |mov ecx, [eax + 4] ;; get addr to subclass table for rhs
                         |pop edx ;; get pointer to lhs WE MUST PUT THIS BACK ON THE STACK
                         |mov ebx, [edx]
                         |mov edx, [ebx + 8] ;; get offset of subclass table for lhs
                         |push edx ;; put back lhs pointer
                         |push eax ;; save rhs value
                         |mov eax, 0xffffffff
                         |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                         |je .array_subclass_check_pass${myCounter}
                         |.array_subclass_check_pass${myCounter}
                         |pop eax ;; restore rhs value, finished
                e check
           """.stripMargin)
                  case _ => ASM(s"""""")
                }
              } else {
                ASM(s"""""")
              }
          }

        ASM(s";; ${assignment.getLHS} := ${assignment.getRHS}") ++
          ASM(s";; := eval lhs") ++
          lhsCode ++
          ASM(s"push eax ;; assignment: pushing lhs l-value to stack") ++
          ASM(s";; := eval rhs") ++
          rhsCode ++
          ASM(
            s""";; := eax has rhs value (addr for obj, value for prim), ebx has rhs pointer (to stack addr)""") ++
          subTypeCheckCode ++
          ASM(s";; lhs := rhs") ++
          ASM(s"""
             |;; assignment: assume value from rhs in eax
             |pop ebx         ;; assignment: popping lhs from stack
             |mov [ebx], eax  ;; assignment: storing rhs value into lhs
           """.stripMargin)
      case Some(name: Name) =>
        NameASM.nameASM(Some(name), lvalue)
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH methodASM on $ast")
        CommonASM.commonASM(Some(ast), MethodASM.methodASM, lvalue)
      case None => ASM("")
      case _    => throw new MatchError(s"methodASM match error on $ast")
    }
  }
}
