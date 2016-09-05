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
    *
    * Exercise 1
    *
    */

  def pascal(c: Int, r: Int): Int = {

    def isEdge(c:Int, r:Int):Boolean = c == 0 || c == r

    if (isEdge(c,r)) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  }

  /**
    *
    * Exercise 2
    *
    */

  def balance(chars: List[Char]): Boolean = {

    def countOpenParen(char: Char, count: Int): Int = {
      if(char == '(') return count + 1

      if(char == ')') return count - 1

      count

    }

    def isBalanced(chars: List[Char], openParens: Int):Boolean = {
      if (chars.head == ')' && openParens == 0) return false

      if (chars.tail.isEmpty)
        countOpenParen(chars.head, openParens) == 0
      else
        isBalanced(chars.tail, countOpenParen(chars.head, openParens))

    }

    isBalanced(chars, 0)

  }



  /**
    *
    * Exercise 3
    *
    */

  def countChange(money: Int, coins: List[Int]): Int = {

    if (coins.isEmpty || money < 0) return 0

    if (money == 0)
      return 1
    else
      return countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}