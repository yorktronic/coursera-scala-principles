/* This file contains practice code for week 1 */

object session {
  // Absolute value
  def abs(x: Double) = if (x < 0) -x else x

  // Greatest common denominator using Euclidian method
  def gcd(a: Int, b: Int): Int =
    if (b==0) a else gcd(b, a % b)

  // Factorial
  def factorial(n: Long): Long =
    if (n==0) 1 else n * factorial(n-1)

  // Factorial (tail-recursive)
  def factorial_tail(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc*n, n-1)
    loop(1, n)
  }

  // Square root using Newtonian method
  def sqrt(x: Double) = { // uses a block. Blocks are themselves expressions in Scala
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1.0)
  }

}

