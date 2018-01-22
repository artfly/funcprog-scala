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
    val left = if (c - 1 >= 0) pascal(c - 1, r - 1) else 0
    val right = if (r - 1 >= c) pascal(c, r - 1) else 0
    if (left + right > 0) left + right else 1
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def brackets(count: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) count
      else if (chars.head == '(') brackets(count + 1, chars.tail)
      else if (chars.head == ')')
        if (count - 1 < 0) count - 1 else brackets(count - 1, chars.tail)
      else brackets(count, chars.tail)
    }

    brackets(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def change(count: Int, money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) count
      else if (money - coins.head == 0) change(count + 1, money, coins.tail)
      else if (money - coins.head > 0) change(change(count, money - coins.head, coins), money, coins.tail)
      else change(count, money, coins.tail)
    }

    change(0, money, coins)
  }
}
