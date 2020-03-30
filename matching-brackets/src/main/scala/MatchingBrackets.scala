import scala.collection.mutable.Stack

object MatchingBrackets {
  private val bracketsMapping = Map(
    ']' -> '[',
    '}' -> '{',
    ')' -> '('
  )

  def isPaired(expression: String): Boolean = {
    val stack = new Stack[Char]
    var isValid: Boolean = true

    expression.takeWhile(currentChar => {
      currentChar match {
        case '[' | '{' | '(' => stack.push(currentChar)
        case ']' | '}' | ')' =>
          isValid = (stack.nonEmpty && this.bracketsMapping(currentChar) == stack.pop)
        case _ => Unit
      }

      isValid
    })

    isValid && stack.isEmpty
  }

}
