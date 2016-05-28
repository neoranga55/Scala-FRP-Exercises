object session {
  1 + 4
  def sqrt(x: Double) = {
    def abs(d: Double) = if (d < 0) -d else d
    def isGoodEnough(guess: Double) =
      abs((guess * guess) - x) / x < 0.0000001
    def improve(guess: Double) =
      (guess + (x / guess)) / 2
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    sqrtIter(1.0)
  }
  sqrt(2)
  sqrt(3)
  sqrt(1e-6)
  sqrt(1e60)
  0.001
  0.1e-20
  1.0e20
  1.0e50
// Factorial
  def factorial(x: Int): Int = {
    def factorialTailRec(accu: Int, i: Int): Int = {
      if (i == 0) accu
      else factorialTailRec(accu * i, i - 1)
    }
    factorialTailRec(1, x)
  }
  factorial(4)
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f) (a + 1, b)
  }
  def fact(n: Int): Int = product(x => x) (1, n)
  product(x => x * x)(3,4)
  fact(4)

  // AKA mapReduce
  def serialOperation(f: Int => Int, op: (Int, Int) => Int, base: Int)(a: Int, b: Int): Int = {
    if (a > b) base
    else op(f(a), serialOperation(f,op, base)(a + 1, b))
  }
  serialOperation(x => x + x, (x, y) => x * y, 1) (2, 3) // Product
  serialOperation(x => x + x, (x, y) => x + y, 0) (2, 3) // Sum

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

}