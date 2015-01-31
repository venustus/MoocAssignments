/**
 * Created by venkat on 28/05/14.
 */


object rationals {
    class Rational(x: Int, y: Int) {
        require(y != 0, "denominator must not be zero")
        private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
        val numer = x
        val denom = y

        def this(x: Int) = this(x, 1)

        def +(that: Rational) =
            new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

        def unary_- = new Rational(-numer, denom)

        def -(that: Rational) = this + -that

        def <(that: Rational) = numer * that.denom < that.numer * denom

        def max(that: Rational) = if(this < that) that else this

        override def toString = (numer / gcd(numer, denom)) + "/" + (denom / gcd(numer, denom))
    }
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)
    x - y - z
    y + y
    x < y
    x max y
    new Rational(2)
}
