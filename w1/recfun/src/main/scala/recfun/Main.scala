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
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    // First, reduce chars to only contain parenthesis
    // Check to see if chars contains a closing parenthesis before an opening one?
    // Get indexes of all closing and opening parenthesis then compare the lists. If there's ever an instance where the index of close is less than the instance of open, return false
    // Have an incrementer only for open parenthesis, which is
      def loop(acc: Int, chars: List[Char]): Boolean =
        if (acc < 0) false
        else if ((chars.isEmpty) && (acc > 0)) false
        else {
          if (chars.head == "(") loop(acc + 1, chars.tail)
          else if (chars.head == ")") loop (acc - 1, chars.tail)
          else loop(acc, chars.tail)
        }



    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
}
