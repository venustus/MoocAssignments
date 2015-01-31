/**
 * Created by venkat on 25/05/14.
 */

object session {
    def abs(x: Double) = if (x < 0) -x else x
    def sqrt(x: Double) = {
        def sqrtIter(guess: Double): Double =
            if(isGoodEnough(guess)) guess
            else sqrtIter(improve(guess))
        def isGoodEnough(guess: Double) =
            abs(guess * guess - x) / x < 0.001
        def improve(guess: Double) =
            (guess + x / guess) / 2
        sqrtIter(1.0)
    }
    sqrt(2)
    sqrt(4)
    sqrt(1e-6)
    sqrt(1e60)
    def factorial(n: Int): Int = {
        def factAcc(n: Int, acc: Int): Int =
            if(n == 0) acc
            else factAcc(n-1, acc * n)
        factAcc(n, 1)
    }

    factorial(5)
}