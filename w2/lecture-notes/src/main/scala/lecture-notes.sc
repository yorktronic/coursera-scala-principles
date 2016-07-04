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

def sum_tail_recursive(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

sum_tail_recursive(x => x * x, 1, 3)






