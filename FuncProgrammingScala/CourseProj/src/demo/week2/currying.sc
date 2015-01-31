/**
 * Created by venkat on 28/05/14.
 */

object currying {
    def abs(x: Double) = if (x < 0) -x else x
    def product(f:Int => Int)(a: Int, b: Int): Int =
        if(a > b) 1 else f(a) * product(f)(a + 1, b)
    product(x => x * x)(3, 4)
    def factorial(a: Int): Int =
        product(x => x)(1, a)
    factorial(5)
    def intervalOp(f:Int => Int, op:(Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
        if(a > b) unit else op(f(a), intervalOp(f, op, unit)(a + 1, b))
    intervalOp(x => x * x, (x, y) => x * y, 1)(3, 4)
    intervalOp(x => x * x, (x, y) => x + y, 0)(3, 4)
    val tolerance = 0.0001
    def isCloseEnough(x: Double, y: Double) =
        abs((x - y) / x) / x < tolerance

    def fixedPoint(f: Double => Double)(firstGuess: Double) = {
        def iterate(guess: Double): Double = {
            val next = f(guess)
            if(isCloseEnough(guess, next)) next
            else iterate(next)
        }
        iterate(firstGuess)
    }

    def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

    def sqrt(x: Double) = fixedPoint(averageDamp(y => (x / y)))(1.0)


    sqrt(2)
}