package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    // row = 0 && col = 0 => 0
    if(r == 0 && c == 0) 1
    // row < 0 && col < 0 = 0
    else if(r < 0 || c < 0) 0

    // row r col c = pascal(c, r-1) + pascal(c-1, r-1)
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParenthesis(chars: List[Char], currentCount: Int): Int = {
      // If we ever see currentCount below 0 that means
      // there are unmatched closing parenthesis.
      if(currentCount < 0) -1

      // We reached to end of character list.
      // What we finally found is the count of parenthesis.
      else if(chars.isEmpty) currentCount

      // If first element in list is opening or closing parenthesis
      // change parenthesis count value accordingly
      else if(chars.head == '(') countParenthesis(chars.tail, currentCount + 1)
      else if(chars.head == ')') countParenthesis(chars.tail, currentCount - 1)

      // Its not parenthesis.
      // Continue to search list
      else countParenthesis(chars.tail, currentCount)
    }

    countParenthesis(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
