package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(countChange(25, List(5, 10, 25)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def findParens(open: Int, close: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) close == open
        else if (close > open) { false }
        else if (chars.head == '(') { findParens(open + 1, close, chars.tail) }
        else if (chars.head == ')') { findParens(open, close + 1, chars.tail) }
        else { findParens(open, close, chars.tail) }
      }
      findParens(0, 0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (coins.head > money) 0
      else if (coins.head == money) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
