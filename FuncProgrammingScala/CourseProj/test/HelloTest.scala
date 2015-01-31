import demo.Hello
import org.scalatest.FunSuite

/**
 * Created by venkat on 25/05/14.
 */
class HelloTest extends FunSuite {
    test("sayHello method works correctly") {
        val hello = new Hello
        assert(hello.sayHello("Scala") == "Hello, Scala!")
    }

}
