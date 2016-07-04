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
      // recursively call pascal until you get the sum of the values above c and r
      // values at edges are always 1
      // values where row = column are always 1
      if ((c == 0) || (c == r)) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2: Parenthesis Balancing
   */
    def balance(chars: List[Char]): Boolean = {
    // Takes in list of characters and returns true if the list has no un-terminated parenthesis
      def loop(acc: Int, chars: List[Char]): Boolean =
        if (acc < 0) false
        else {
          if (chars.isEmpty) {
            if (acc > 0) false
            else true
          }
          else {
            if (chars.head == '(') loop(acc + 1, chars.tail)
            else if (chars.head == ')') loop(acc - 1, chars.tail)
            else loop(acc, chars.tail)
          }
        }
      loop(0, chars)
    }
  
  /**
   * Exercise 3: Counting Change
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if ((money < 0) || ((money > 0) && coins.isEmpty)) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
}
