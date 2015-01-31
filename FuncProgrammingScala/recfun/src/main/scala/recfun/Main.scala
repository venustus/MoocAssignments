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
    if(c == 0 && r == 0) 1
    else {
        val v1 = if(c - 1 < 0) 0 else pascal(c - 1, r - 1)
        val v2 = if(c > r - 1) 0 else pascal(c, r - 1)
        v1 + v2
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCount(chars: List[Char], b: Int): Boolean =
        chars match {
            case List() => b == 0
            case _ =>  {
                if(b < 0) false
                else {
                    if(chars.head == '(')  balanceCount(chars.tail, b + 1)
                    else if(chars.head == ')') balanceCount(chars.tail, b - 1)
                    else balanceCount(chars.tail, b)
                }
            }
        }

    balanceCount(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeMultiple(money: Int, head: Int, tail: List[Int], y: Int): Int =
        if(money - y * head == 0) 1
        else if(money - y * head < 0) 0
        else countChange(money - y * head, tail) + countChangeMultiple(money, head, tail, y + 1)
    coins match {
        case x :: xs => countChange(money, xs) + countChangeMultiple(money, x, xs, 1)
        case List() => 0
    }
  }
}
