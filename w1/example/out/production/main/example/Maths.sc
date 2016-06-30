/* This file contains coding notes for week 1 of Coursera's Functional Programming in Scala - Functional Programming Principles */

object session {
  /* Square root method using Newton's methond */
  def sqrtIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else sqrIter(improve(guess, x), x)


  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) < 0.0001

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

}
