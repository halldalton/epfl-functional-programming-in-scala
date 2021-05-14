package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println("Balance")
    balance("(if (zero? x) max (/ 1 x))".toList)
    println()
    println("Count Change")
    countChange(4, List(1, 2, 3))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      @tailrec
      def loop(acc: Int, n: Int): Int = {
        if (n == 0) acc else loop(acc * n, n - 1)
      }
      loop(1, n)
    }
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char]): Int = {
      def checkChar(char: Char): Int = if (char == '(') 1 else if (char == ')') -1 else 0
      if (chars.isEmpty) 0
      else {
        val value = checkChar(chars.head) + checkBalance(chars.tail)
        if (value > 0) -1 else value
      }
    }
    if(checkBalance(chars) == 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if(money < 0 || coins.isEmpty) 0
    else {
      val included = countChange(money - coins.head, coins)
      val excluded = countChange(money, coins.tail)
      included + excluded
    }
  }
}
