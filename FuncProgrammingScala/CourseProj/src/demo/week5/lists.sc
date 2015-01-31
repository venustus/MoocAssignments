/**
 * Created by venkat on 07/06/14.
 */


object Lists {
    def mapFun[T, U](xs: List[T], f: T => U): List[U] =
        (xs foldRight List[U]())((x, acc) => f(x) :: acc)
    def lengthFun[T](xs: List[T]): Int =
        (xs foldRight 0)((x, acc) => acc + 1)

    def squareList(xs: List[Int]):List[Int] = xs match {
        case Nil => xs
        case y :: ys => y * y :: squareList(ys)
    }
    def squareListMap(xs: List[Int]): List[Int] = xs map ((x) => x * x)
    squareList(List(1, 2, 3, 4))
    squareListMap(List(1, 2, 3, 4))
    def pack[T](xs: List[T]): List[List[T]] = xs match {
        case Nil => Nil
        case x :: xs1 => (x :: (xs1 takeWhile (y => y == x))) :: pack(xs1 dropWhile (y => y == x))
    }
    def encode[T](xs: List[T]): List[Pair[T, Int]] = {
        mapFun(pack(xs), (x: List[T]) => (x.head, x.length))
    }

    pack(List("a", "a", "a", "b", "c", "c", "a"))
    encode(List("a", "a", "a", "b", "c", "c", "a"))
    lengthFun[String](List("a", "a", "a", "b", "c", "c", "a"))

}