package compiler.joos1w.asm

import com.sun.jdi.ByteType
import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment.environment
import compiler.joos1w.environment.types.numeric.{CharType, IntType, ShortType}
import compiler.joos1w.environment.types.{
  AbstractType,
  BooleanType,
  CustomType,
  StringType
}

object ExpressionASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def getStringValueOfMethodLabel(ttype: AbstractType): String = {
    ttype match {
      case ttype: IntType     => "java_lang_String_valueOf_int"
      case ttype: CharType    => "java_lang_String_valueOf_char"
      case ttype: ShortType   => "java_lang_String_valueOf_short"
      case ttype: ByteType    => "java_lang_String_valueOf_byte"
      case ttype: BooleanType => "java_lang_String_valueOf_boolean"
      case ttype: CustomType  => "java_lang_String_valueOf_Object"
      case ttype: StringType  => "java_lang_String_valueOf_String"
    }
  }

  def generalExpressionASM(expr: GeneralExpression,
                           recurseMethod: (Option[AST], Boolean) => ASM,
                           lvalue: Boolean): ASM = {
    expr.operation match {
      case Some(op) =>
        val expr1Code = recurseMethod(Some(expr.firstExpr), lvalue)
        val expr2Code = recurseMethod(Some(expr.secondExpr), lvalue)
        op match {
          case "+" =>
            val lhsType = environment.determineType(expr.firstExpr, expr.env)
            val rhsType = environment.determineType(expr.secondExpr, expr.env)

            if (lhsType.isString || rhsType.isString) { // handle string addition
              // determine which one is a string
              // for the other one break into cases: String, Primitive, Object

              // gets string values of both LHS and RHS into eax and ebx
              val getStringValuesASM =
                if (lhsType.isString && !rhsType.isString) { // only lhs string
                  val valueOfMethodLabel = getStringValueOfMethodLabel(rhsType)
                  ASM(
                    s";; BEGIN STRING ADDITION OF TYPES: ${lhsType}  +  ${rhsType}")
                  expr2Code ++
                    ASM(s"""
                       |push 0 ;; push nothing as this reference for static method
                       |push eax ;; push value of RHS
                       |call ${valueOfMethodLabel}
                       |add esp, 8
                       |;; ebx should now have pointer to string value of rhs, save it, and then get LHS string
                       |push ebx
                       |""".stripMargin) ++
                    expr1Code ++
                    ASM(s"""
                       |mov eax, ebx
                       |pop ebx ;; eax now has string pointer of LHS, ebx has string pointer of RHS
                       |;; now we concat the two strings""".stripMargin)

                } else if (!lhsType.isString && rhsType.isString) { // only rhs string
                  val valueOfMethodLabel = getStringValueOfMethodLabel(lhsType)
                  ASM(
                    s";; BEGIN STRING ADDITION OF TYPES: ${lhsType}  +  ${rhsType}")
                  expr1Code ++
                    ASM(s"""
                           |push 0 ;; push nothing as this reference for static method
                           |push eax ;; push value of LHS
                           |call ${valueOfMethodLabel}
                           |add esp, 8
                           |;; ebx should now have pointer to string value of rhs, save it, and then get LHS string
                           |push ebx
                           |""".stripMargin) ++
                    expr2Code ++
                    ASM(s"""
                           |pop eax ;; eax now has string pointer of LHS, ebx has string pointer of RHS
                           |;; now we concat the two strings""".stripMargin)
                } else { // both string
                  ASM(
                    s";; BEGIN STRING ADDITION OF TYPES: ${lhsType}  +  ${rhsType}")
                  expr1Code ++
                    ASM(s"""
                           |push ebx ;; save lhs string pointer
                           |""".stripMargin) ++
                    expr2Code ++
                    ASM(s"""
                           |pop eax ;; eax now has string pointer of LHS, ebx has string pointer of RHS
                           |;; now we concat the two strings""".stripMargin)
                }

              // we now have eax as LHS string pointer, ebx as RHS string pointer, and perform concat
              return getStringValuesASM ++
                ASM(s"""
                       |;; TODO concat strings
                       |push eax
                       |push ebx
                       |call java_lang_String_concat_String
                       |add esp, 8
                       |""".stripMargin)
            }

            ASM(s";; begin ${expr.firstExpr} + ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |add eax, ebx
                 |;; end ${expr.firstExpr} + ${expr.secondExpr}
                 |""".stripMargin)
          case "-" =>
            ASM(s";; ${expr.firstExpr} - ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |mov ebx, eax
                 |pop eax
                 |sub eax, ebx
                 |;; end ${expr.firstExpr} - ${expr.secondExpr}
                 |""".stripMargin)
          case "*" =>
            ASM(s";; ${expr.firstExpr} * ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |imul eax, ebx
                 |;; end ${expr.firstExpr} * ${expr.secondExpr}
                 |""".stripMargin)
          case "&" =>
            ASM(s";; ${expr.firstExpr} & ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                     |pop ebx
                     |and eax, ebx
                     |;; end ${expr.firstExpr} * ${expr.secondExpr}
                     |""".stripMargin)
          case "|" =>
            ASM(s";; ${expr.firstExpr} | ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                     |pop ebx
                     |or eax, ebx
                     |;; end ${expr.firstExpr} * ${expr.secondExpr}
                     |""".stripMargin)
          case "/" =>
            ASM(s";; ${expr.firstExpr} / ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              // TODO something about the sign
              ASM(s"""
                 |mov ebx, eax
                 |pop eax
                 |mov edx, 0
                 |div ebx
                 |;; end ${expr.firstExpr} / ${expr.secondExpr}
                 |""".stripMargin)
          case "%" =>
            ASM(s";; ${expr.firstExpr} % ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              // TODO something about the sign
              ASM(s"""
                     |mov ebx, eax
                     |pop eax
                     |mov edx, 0
                     |div ebx
                     |mov eax, edx ;; get remainder into eax from edx
                     |;; end ${expr.firstExpr} % ${expr.secondExpr}
                     |""".stripMargin)
          case "instanceof" =>
            val myCounter = incrementAndReturnCounter
            val rhs = environment.determineType(expr.secondExpr, expr.env)
            val rhsTypeEnv = rhs match {
              case rhs: StringType => rhs.env
              case rhs: CustomType => rhs.env
            }

            ASM(s";; ${expr.firstExpr} instanceof( ${expr.secondExpr} )") ++
              ASM(";; get left side instanceof") ++
              expr1Code ++
              ASM(s"""
                     |push eax ;; instanceof: push lhs obj ref
                     |         ;; TODO? array ref
                     |;; get instanceof right sides class into ebx
                    """.stripMargin) ++
              expr2Code ++
              ASM(s"""
                     |;; now esp has pointer to lhs, eax has pointer to rhs
                     |;; eax should be a class addr
                     |;; perform actual instance of, eax has lhs class, ebx has rhs class
                     |mov ecx, [eax + 8] ;; get offset of subclass table for rhs
                     |pop ebx            ;; ebx <- lhs obj ref
                     |mov ebx, [ebx]     ;; ebx <- class(ebx)
                     |mov edx, [ebx + 4] ;; get offset to subclass table for lhs
                     |mov eax, 0xffffffff
                     |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                     |mov eax, 0xffffffff
                     |je .instanceof_subtype_check_pass${myCounter}
                     |mov eax, 0
                     |.instanceof_subtype_check_pass${myCounter}:""".stripMargin)

        }
      case _ => throw new MatchError(s"Expression match error")
    }
  }

  def unaryExpressionASM(expr: UnaryExpression,
                         recurseMethod: (Option[AST], Boolean) => ASM,
                         lvalue: Boolean): ASM = {
    val exprCode = recurseMethod(Some(expr.subExpression), lvalue)

    expr.operator match {
      case "!" =>
        ASM(";; ${expr.operator} ${expr.subExpression}") ++
          exprCode ++
          ASM(s"""
               |not eax
               |""".stripMargin)
      case "-" =>
        ASM(";; ${expr.operator} ${expr.subExpression}") ++
          exprCode ++
          ASM(s"""
               |sub eax, eax
               |sub eax, eax
               |""".stripMargin)
      case s => throw new MatchError(s"TODO IMPLEMENT $s")
    }
  }

  def conditionalExpressionASM(expr: ConditionalExpression,
                               recurseMethod: (Option[AST], Boolean) => ASM,
                               lvalue: Boolean): ASM = {
    expr.operator match {
      case op =>
        val expr1Code = recurseMethod(Some(expr.firstExpr), lvalue)
        val expr2Code = recurseMethod(Some(expr.secondExpr), lvalue)
        op match {
          case "!=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} != ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jne .end_not_equal${myCounter}
                   |mov ebx, 0
                   |.end_not_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "==" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} == ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 0
                   |jne .end_equal_equal${myCounter}
                   |mov ebx, 1
                   |.end_equal_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} >= ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jge .end_greater_equal${myCounter}
                   |mov ebx, 0
                   |.end_greater_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} > ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jg .end_greater${myCounter}
                   |mov ebx, 0
                   |.end_greater${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} <= ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jle .end_less_equal${myCounter}
                   |mov ebx, 0
                   |.end_less_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} < ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jl .end_less${myCounter}
                   |mov ebx, 0
                   |.end_less${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "&&" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} && ${expr.secondExpr}") ++
              expr1Code ++
              ASM(s"""
                   |cmp eax, 0
                   |je .end_and${myCounter}
                 """.stripMargin) ++
              expr2Code ++
              ASM(s".end_and${myCounter}:")
          case "||" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} || ${expr.secondExpr}") ++
              expr1Code ++
              ASM(s"""
                   |cmp eax, 0xffffffff
                   |je .end_and${myCounter}""".stripMargin) ++
              expr2Code ++
              ASM(s".end_and${myCounter}:")

        }
    }
  }
}
