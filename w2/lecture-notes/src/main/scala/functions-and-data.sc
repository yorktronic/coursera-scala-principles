/*
 * A Rational is a fraction
 * Scala keeps the names of types and values in different namespaces.
 * So, there's no conflict between the two definitions of Rational
 */
class Rational(x: Int, y: Int) {

  // Prevent divide by zero errors
  require(y != 0, "denominator must be nonzero")

  // Allow a Rational to be constructed with a numerator only
  def this(x: Int) = this(x, 1)

  // Greatest common denominator (GCD) function
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  // Numerator and denominator (automatically reduced to gcd)
  def numer = x / g
  def denom = y / g

  /*
   * Could also be represented as the following so that numer and denom are computed only once
   *
   * val numer = x / gcd(x, y)
   * val denom = y / gcd(x, y)
   *
   * This would be advantageous if numer and denom were called very often
   */

  // Gets the negative of a rational
  def neg: Rational = new Rational(-numer, denom)

  // Determines if this is less than that
  def less(that: Rational) = numer * that.denom < that.numer * denom

  // Returns the max of two rationals
  def max(that: Rational) = if (this.less(that)) that else this

  // Adds two Rationals
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  // Subtracts a passed in rational from a rational
  def sub(that: Rational) = add(that.neg)

  // Pretty prints rationals
  override def toString = numer + "/" + denom // need the override since we override the method built-in to Scala

}

object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(189, 23)
}

"Subtracting " + rationals.x.toString + " from " + rationals.z.toString + " = " + rationals.z.sub(rationals.x).toString



