/*
 * Exploring ways of storing data in Scala
 */

object rationals {
  val x = new Rational(1,2)
  val y = new Rational(2, 3)
}

/*
 * Rational is type in this case that is a fraction
 * Scala keeps the names of types and values in different namespaces.
 * So, there's no conflict between the two definitions of Rational
 */
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
}

// Adds two rationals together and returns the result
def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom)

def makeString(r: Rational) =
  r.numer + "/" + r.denom

makeString(addRational(rationals.x, rationals.y))