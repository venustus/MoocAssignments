package simulations

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

/**
 * Created by venkat on 04/01/15.
 */
object QuickCircuit extends Properties("Circuit") {

    import Circuit.{demux,run}

    val genBoolean = Gen.oneOf(Gen.value(true), Gen.value(false))
    val genShortLitBoolean = for {
        n <- Gen.choose(0, 8) // More than 8 gets slooooooooooow ...
        g <- Gen.listOfN(n, genBoolean)
    } yield g

    def signalToBit(s: Boolean) =
        if (s) 1 else 0

    def controlToBitNumber(bs: List[Boolean]) =
        bs.foldLeft(0){
            case (acc, b) => 2 * acc + signalToBit(b) }

    def setWiresToSignals(ws: List[Wire], ss: List[Boolean]) =
        (ws zip ss) foreach { case (w, s) => w setSignal s }

    def countOneBits(ws: List[Wire]) =
        ws.map(_.getSignal).filter(identity).length

    property("demux") = forAll (genBoolean, genShortLitBoolean) {
        (sIn: Boolean, sControl: List[Boolean]) =>
            val numBitsControl = sControl.length
            val numBitsOutput = 1 << numBitsControl
            val wIn = new Wire
            val wControl = List.fill(numBitsControl){new Wire}
            val wOutput = List.fill(numBitsOutput){new Wire}

            demux(wIn, wControl, wOutput)
            wIn.setSignal(sIn)
            setWiresToSignals(wControl, sControl)
            run

            val numBitInOut = numBitsOutput - 1 - controlToBitNumber(sControl)
            val targetBitShouldBeInput = wOutput(numBitInOut).getSignal == sIn

            targetBitShouldBeInput && countOneBits(wOutput) == signalToBit(sIn)
    }
}
