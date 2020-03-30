import scala.collection.mutable.Stack

object MatchingBracketsRecursion {
  private val bracketsMapping = Map(
    ']' -> '[',
    '}' -> '{',
    ')' -> '('
  )

  def isPaired(expression: String): Boolean = {
    def verifyBracketPairing(i: Int, isValid: Boolean, stack: Stack[Char]): Boolean = {
      if (!isValid || i >= expression.length()) return isValid

      val currentChar = expression.charAt(i)

      verifyBracketPairing(
        i + 1,
        isValid && isBracketMatching(currentChar, stack),
        processOpenBrackets(currentChar, stack))
    }

    val stack = new Stack[Char]
    verifyBracketPairing(0, true, stack) && stack.isEmpty
  }

  private def processOpenBrackets(bracket: Char, stack: Stack[Char]): Stack[Char] = {
    bracket match {
      case '[' | '{' | '(' => stack.push(bracket)
      case _ => stack
    }
  }

  private def isBracketMatching(bracket: Char, stack: Stack[Char]): Boolean = {
    bracket match {
      case ']' | '}' | ')' =>
        (!stack.isEmpty && this.bracketsMapping(bracket) == stack.pop)
      case _ => true //Any other character ignore
    }
  }
}
