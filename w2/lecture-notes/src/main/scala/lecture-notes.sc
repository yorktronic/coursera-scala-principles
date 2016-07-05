/*
 * Doing this without anonymous functions
 */
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def sumInts(a: Int, b: Int) = sum(id, a, b)
def sumCubes(a: Int, b: Int) = sum(cube, a, b)
def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

sumFactorials(4, 8)

/*
 * With anonymous functions
 */
(x: Int) => x * x * x // cube function
(x: Int, y: Int) => x + y // sum function
def sumInts_anonymous(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes2_anonymous(a: Int, b: Int) = sum (x => x * x * x, a, b)

object exercise2 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }
}

exercise2.sum(x => x * x, 1, 3)

object exercise3 {
  def sum(f: Int => Int): (Int, Int) => Int = { // sum is now a function that returns another function. The type of sum is Int
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }
  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumFactorials = sum(fact)
  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)
  def cube(x: Int): Int = x * x * x
}

exercise3.sumCubes(1, 5)
exercise3.sum (cube) (1, 5)
exercise3.sum (cube) (1, 5) == (exercise3.sum (cube)) (1, 5)

/*
 * Modify the codein exercise3 to create a product function
 * Also create a factorial function that uses the product function
 */
object exercise4 {
  def product(f: Int => Int): (Int, Int) => Int = {
    def productF(a: Int, b: Int): Int =
      if (a > b) 1
      else f(a) * productF(a + 1, b) // f(a) can also be just a in this case (I guess the instructor used f(a) to deliberately confuse me?)
    productF
  }
  def fact(x: Int): Int = product(x => x)(1, x)
}

exercise4.product(x => x)(1, 4)
exercise4.fact(4)

/*
 * Generalizing sum and product into a new function called mapReduce
 */
object exercise5 {
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
  def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
  def fact(x: Int) = product(x => x)(1, x)
}

exercise5.sum(x => x)(1, 4)
exercise5.fact(5)

/*
 * Finding fixed points of a function
 * The fixed point is when f(x) = x
 */
import math.abs

object exercise6 {
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {

    def iterate(guess: Double): Double = {
      println("guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  /*
   * We can now re-write our square root function using the fixed point function
   * Recall: sqrt(x) = the number y such that y * y = x
   * Divide both sides by y: sqrt(x) = the number y such that y = x / y
   * So, sqrt(x) is a fixed point of the function (y => x / y)
   * However, def sqrt(x: Double) = fixedPoint(y => x / y)(1.0) doesn't work, because it will never converge
   * We need to use average damping (averaging successive values)
  */
  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)

  // We then generalize the averageDamp function
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  // And re-write sqrt as a function of fixedPoint and averageDamp
  def sqrt2(x: Double) = fixedPoint(averageDamp(y => x / y))(1)
}

// exercise6.fixedPoint(x => 1 + x / 2)(1)
exercise6.sqrt2(1298745)

