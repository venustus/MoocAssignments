/**
 * Created by venkat on 09/06/14.
 */

object collections {
    def isPrime(n: Int): Boolean =
        (2 until n) forall (x => n%x != 0)
    isPrime(17)

    isPrime(58)

    def scalarProduct(xs: List[Double], ys: List[Double]): Double =
        (for{(x, y) <- xs zip ys} yield x * y) sum
}