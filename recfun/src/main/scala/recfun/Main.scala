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
  def pascal(c: Int, r: Int): Int =
    if (r < 0 || c < 0 || c > r) {
      0
    }
    else if (r == 0) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], count: Integer): Integer =
      if (chars.isEmpty || count < 0)
        count
      else if (chars.head.equals('('))
        innerBalance(chars.tail, count + 1)
      else if (chars.head.equals(')'))
        innerBalance(chars.tail, count - 1)
      else
        innerBalance(chars.tail, count)
    innerBalance(chars, 0) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCount(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) {
        0
      }
      else if (money == coins.head) {
        1 + innerCount(money, coins.tail)
      }
      else {
        innerCount(money - coins.head, coins) + innerCount(money, coins.tail)
      }
    }
    if (money == 0) {
      1
    }
    else if (money < 0 || coins.isEmpty) {
      0
    }
    else
      innerCount(money, coins)
  }
}
