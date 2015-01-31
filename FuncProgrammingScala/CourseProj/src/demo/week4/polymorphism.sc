import demo.week4.{List, Nil, Cons}
/**
 * Created by venkat on 31/05/14.
 */

object Main {
    def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
    def nth[T](n: Int, l: List[T]): T = {
        if(l.isEmpty) throw new IndexOutOfBoundsException("nth called on empty list")
        if(n == 0) l.head
        else nth(n - 1, l.tail)
    }
    singleton[Int](1)
    singleton[Boolean](true)

    nth(2, new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil[Int]))))
}