package joos1w

import org.scalatest.FunSuite

/**
  * Applicable checks for reachability based off of JLS 2.0 Chapter 14.20
  */
class ReachabilityTest extends FunSuite {
  test("""
      | The block that is the body of a constructor, method, instance
      | initializer or static initializer is reachable
    """.stripMargin) {}

  test(
    """
      | A local class declaration statement can complete normally iff it is reachable.
    """.stripMargin) {}

  test(
    """
      | A local variable declaration statement can complete normally iff it is reach-able.
    """.stripMargin) {}

  test("""
      | An empty statement can complete normally iff it is reachable.
    """.stripMargin) {}

  test("""
         | An empty statement can complete normally iff it is reachable.
       """.stripMargin) {}

  test("""
         | An empty statement can complete normally iff it is reachable.
       """.stripMargin) {}

  test(
    """
      | A while statement can complete normally iff at least one of the following is true:
      | - The while statement is reachable and the condition expression is not a constant
      |   expression with value true.
      | - There is a reachable break statement that exits the while statement.
      | 
      | The contained statement is reachable iff the while statement is reachable and the
      | condition expression is not a constant expression whose value is false.
    """.stripMargin) {}

  test(
    """
      | A for statement can complete normally iff at least one of the following is true:
      | - The for statement is reachable, there is a condition expression, and the condition
      |   expression is not a constant expression with value true.
      | - There is a reachable break statement that exits the for statement. The contained
      |   statement is reachable iff the for statement is reachable and the condition expression
      |   is not a constant expression whose value is false.
    """.stripMargin) {}

  test("""
      | A break, continue, or return statement cannot complete normally.
    """.stripMargin) {}

  test(
    """
      | An if–then statement can complete normally iff it is reachable. The then–statement is 
      | reachable iff the if–then statement is reachable.
    """.stripMargin) {}

  test(
    """
      | An if–then–else statement can complete normally iff the then–statement can complete
      | normally or the else–statement can complete normally. The then-statement is reachable
      | iff the if–then–else statement is reachable. The else-statement is reachable iff the 
      | if–then–else statement is reachable.
    """.stripMargin) {}
}