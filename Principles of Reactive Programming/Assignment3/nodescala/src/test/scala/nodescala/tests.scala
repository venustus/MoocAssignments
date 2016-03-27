package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all combinator should result in success if all futures succeed") {
      val f = Future.all((1 to 500).toList map (Future.always(_)))
      assert(Await.result(f, 1 second) == (1 to 500).toList)
  }

  test("all combinator should result in failure if at least one of the futures fail") {
      val p = Promise[Int]()
      p failure (new Exception("Failed promise"))
      val f = Future.all(p.future :: ((1 to 500).toList map (Future.always(_))))
      try {
        Await.result(f, 1 second)
        assert(false)
      } catch {
        case t: Exception => assert(t.getMessage == "Failed promise")
      }
  }

  test("all combinator should result in failure if at least one of the futures didn't return in time") {
      val f = Future.all(Future.never :: ((1 to 500).toList map (Future.always(_))))
      try {
          Await.result(f, 1 second)
          assert(false)
      } catch {
          case t: TimeoutException => // ok!
      }
  }

    test("any combinator should result in success for all successful results") {
        val f = Future.any((1 to 500).toList map (Future.always(_)))
        val r = Await.result(f, 1 second)
        println(s"result: $r")
        assert((1 to 500) contains r)
    }

    test("any combinator should result in failure when given a list of futures one of which fails fast") {
        val f = Future.any(List(future { blocking { Thread.sleep(1000) } }, future { blocking { Thread.sleep(1000) } }, Future.failed(new Exception("failed future")) ))
        try {
            Await.result(f, 1 second)
            assert(false)
        } catch {
            case t: Exception => assert(t.getMessage == "failed future")
        }
    }

    test("ask a feature if it is completed now") {
        assert(Future.always(1).now == 1)
        try {
            future { blocking { Thread.sleep(1000) } }.now
            assert(false)
        } catch {
            case t: NoSuchElementException => // Ok
        }
    }

    test("continue with") {
        val x = Future.always[Int](5).continueWith((f: Future[Int]) => {
                Await.result(f, 0 nanos) + 5
        })
        assert(Await.result(x, 1 second) == 10)
        val y = Future.failed[Int](new Exception("failed future")).continueWith((f: Future[Int]) => {
            try {
                Await.result(f, 0 nanos) + 5
            } catch {
                case _ => -1
            }
        })
        assert(Await.result(y, 1 second) == -1)
    }

    test("continue") {
        val x = Future.always[Int](5).continue[Int] {
            case Success(r) => r + 5
            case _ => -1
        }
        assert(Await.result(x, 1 second) == 10)
        val y = Future.failed[Int](new Exception("failed future")).continue {
            case Success(r) => r + 5
            case Failure(th) => -1
        }
        assert(Await.result(y, 1 second) == -1)
    }



  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        println("working")
        blocking {
            Thread.sleep(1000)
        }
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

    test("Call run method on future companion object and unsubscribe") {
        val working = Future.run() { ct =>
            Future {
                while (ct.nonCancelled) {
                    println("run working")
                    blocking {
                        Thread.sleep(1000)
                    }
                }
                println("done")
            }
        }
        /*
        blocking {
            Thread.sleep(5000)
        }
        working.unsubscribe()
        */

        val waitFor5 = Future.delay(5 seconds)
        waitFor5 onSuccess {
            case _ => { println("unsubscribing after 5 seconds"); working.unsubscribe()}
        }
        Await.ready(waitFor5, 8 seconds)
    }

    /*
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }
  */

}




