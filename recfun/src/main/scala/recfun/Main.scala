package recfun

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
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceP(chars: List[Char], leftCount: Int, rightCount: Int): Boolean = {
      if (leftCount >= rightCount && chars.isEmpty) true
      else if (rightCount > leftCount) false
      else if (chars.head != '(' && chars.head != ')') balanceP(chars.tail, leftCount, rightCount) // Ignore other characters
      else if (chars.head == '(') balanceP(chars.tail, leftCount+1, rightCount)
      else if (chars.head == ')') balanceP(chars.tail, leftCount, rightCount+1)
      else false
    }
    balanceP(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money < 0) 0
    else if (money == coins.head && coins.size == 1) 1
    else if (money < coins.head && coins.size == 1) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
