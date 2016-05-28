object session {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x - y - z
  y + y
  x < y
  x.max(y)
  new Rational(2)

  class Rational(x: Int, y: Int) {
    def this(x: Int) = this(x, 1)
    require(y != 0, "denominator must be nonzero")
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    def numer = x / g
    def denom = y / g

    def max(that: Rational) = if (this < that) that else this

    def <(that: Rational) = numer * that.denom < that.numer * denom

    def +(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    override def toString = if (denom == 1) numer.toString else numer + "/" + denom

//    def neg = new Rational(-numer, denom)// Same as below
    def unary_- = new Rational(-numer, denom)

    def -(that: Rational) = this + -that
  }
}