package compiler.joos1w.asm

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
        ASM(s";; ${v.ttype} ${v.name} := ${v.variableDeclarator}") ++
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
                  Some(ASM(s"""
                       |;; perform array access subtype check
                       |;; assume address to rhs array/obj is in eax
                       |;;        address to lhs array/obj is in esp
                       |pop ebx            ;; get lhs array/obj ref
                       |mov ebx, [ebx]     ;; get cls of array/obj ref
                       |mov ecx, [ebx + 8] ;; get offset of subclass table for lhs
                       |mov ebx, [eax]     ;; get rhs array/obj cls
                       |mov ebx, [ebx + 4] ;; get addr of subclass table for rhs
                       |push eax ;; save rhs value
                       |mov eax, 0xffffffff
                       |cmp eax, [ebx + ecx] ;; check if rhs is subclass of lhs
                       |je .array_subclass_check_pass${myCounter}
                       |call __exception
                       |.array_subclass_check_pass${myCounter}:
                       |pop eax ;; restore rhs value, finished array access sub type check
           """.stripMargin))
                case _ => None
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
                    Some(ASM(s"""
                         |;; perform array access subtype check
                         |;; assume address to rhs array/obj is in eax
                         |;;        address to lhs array/obj is in esp
                         |pop ebx            ;; get lhs array/obj ref
                         |mov ebx, [ebx]     ;; get cls of array/obj ref
                         |mov ecx, [ebx + 8] ;; get offset of subclass table for lhs
                         |mov ebx, [eax]     ;; get rhs array/obj cls
                         |mov ebx, [ebx + 4] ;; get addr of subclass table for rhs
                         |push eax ;; save rhs value
                         |mov eax, 0xffffffff
                         |cmp eax, [ebx + ecx] ;; check if rhs is subclass of lhs
                         |je .array_subclass_check_pass${myCounter}
                         |call __exception
                         |.array_subclass_check_pass${myCounter}:
                         |pop eax ;; restore rhs value, finished array access sub type check
           """.stripMargin))
                  case _ => None
                }
              } else {
                None
              }
          }

        ASM(s";; ${assignment.getLHS} := ${assignment.getRHS}") ++
          ASM(s";; := eval lhs") ++
          lhsCode ++
          ASM(s"push eax ;; assignment: pushing lhs l-value to stack") ++
          (if (subTypeCheckCode.isDefined)
             ASM(s"push ebx ;; assignment: save lhs pointer for subtype check")
           else ASM("")) ++
          ASM(s";; := eval rhs") ++
          rhsCode ++
          ASM(
            s""";; := eax has rhs value (addr for obj, value for prim), ebx has rhs pointer (to stack addr)""") ++
          (if (subTypeCheckCode.isDefined) subTypeCheckCode.get else ASM("")) ++
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
