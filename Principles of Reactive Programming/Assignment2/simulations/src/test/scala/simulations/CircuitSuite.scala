package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite with Checkers {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate tests") {
    def orGateGenericTests(orFunc: (Wire, Wire, Wire) => Unit) = {
      val in1, in2, out = new Wire
      orFunc(in1, in2, out)
      in1.setSignal(true)
      in2.setSignal(true)
      run

      assert(out.getSignal === true, "or 1")

      in1.setSignal(false)
      run

      assert(out.getSignal === true, "or 2")

      in2.setSignal(false)
      run
      assert(out.getSignal === false, "or 3")
    }

    orGateGenericTests(orGate)
    orGateGenericTests(orGate2)
  }

  test("demus basic tests 1 control") {
      val in, c, o0, o1 = new Wire

      demux(in, List(c), List(o1,o0))
      in.setSignal(false)
      c.setSignal(false)
      run

      assert(o0.getSignal == false, "demux1 1")
      assert(o1.getSignal == false, "demux1 1")

      in.setSignal(true)
      run

      assert(o0.getSignal == true, "demux1 2")
      assert(o1.getSignal == false, "demux1 2")

      c.setSignal(true)
      run

      assert(o0.getSignal == false, "demux1 3")
      assert(o1.getSignal == true, "demux1 3")

      in.setSignal(false)
      run

      assert(o0.getSignal == false, "demux1 4")
      assert(o1.getSignal == false, "demux1 4")
  }


  test("demux basic tests 2 controls") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire;
    demux(in, List[Wire](c1, c0), List[Wire](o3, o2, o1, o0))

    in.setSignal(false)

    c0.setSignal(false)
    c1.setSignal(false)

    run

    assert(o0.getSignal === false, "demux2 0 unset")
    assert(o1.getSignal === false, "demux2 1 unset")
    assert(o2.getSignal === false, "demux2 2 unset")
    assert(o3.getSignal === false, "demux2 3 unset")

    in.setSignal(true)

    run

    println(s"${o0.getSignal}, ${o1.getSignal}, ${o2.getSignal}, ${o3.getSignal}")
    assert(o0.getSignal === true, "demux2 0 set ctrl 0 0")
    assert(o1.getSignal === false, "demux2 1 set ctrl 0 0")
    assert(o2.getSignal === false, "demux2 2 set ctrl 0 0")
    assert(o3.getSignal === false, "demux2 3 set ctrl 0 0")

    c1.setSignal(true)
    run

    println(s"${o0.getSignal}, ${o1.getSignal}, ${o2.getSignal}, ${o3.getSignal}")
    assert(o0.getSignal === false, "demux2 0 set ctrl 0 1")
    assert(o1.getSignal === false, "demux2 1 set ctrl 0 1")
    assert(o2.getSignal === true, "demux2 2 set ctrl 0 1")
    assert(o3.getSignal === false, "demux2 3 set ctrl 0 1")

    c1.setSignal(false)
    c0.setSignal(true)
    run

    assert(o0.getSignal === false, "demux2 0 set ctrl 1 0")
    assert(o1.getSignal === true, "demux2 1 set ctrl 1 0")
    assert(o2.getSignal === false, "demux2 2 set ctrl 1 0")
    assert(o3.getSignal === false, "demux2 3 set ctrl 1 0")

    c1.setSignal(true)
    run

    assert(o0.getSignal === false, "demux2 0 set ctrl 1 1")
    assert(o1.getSignal === false, "demux2 1 set ctrl 1 1")
    assert(o2.getSignal === false, "demux2 2 set ctrl 1 1")
    assert(o3.getSignal === true, "demux2 3 set ctrl 1 1")

    in.setSignal(false)
    run

    assert(o0.getSignal === false, "demux2 0 unset")
    assert(o1.getSignal === false, "demux2 1 unset")
    assert(o2.getSignal === false, "demux2 2 unset")
    assert(o3.getSignal === false, "demux2 3 unset")
  }

  test("demux advanced automated tests") {
    check(QuickCircuit)
  }

}
