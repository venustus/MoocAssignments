package org.venustus.cryptography.integrity

import org.scalatest.{Matchers, FlatSpec}
import org.venustus.cryptography.utils.Utils.Hex

/**
 * Created by venkat on 09/02/15.
 */
class StreamingIntegritySpec extends FlatSpec with Matchers {
    "Streaming hash algorithm" should "compute the hash correctly for a sample video file" in {
        val expectedResult = "03c08f4ee0b576fe319338139c045c89c3e8e9409633bea29442e21425006ea8"
        Hex valueOf(StreamingIntegrity computeStreamingHash("/Users/venkat/Documents/Coursera/Cryptography-I/6 - 2 - Generic birthday attack (16 min).mp4")) should be (expectedResult)
    }

    "Streaming hash algorithm" should "compute the hash correctly for a test video file" in {
        Hex valueOf(StreamingIntegrity computeStreamingHash ("/Users/venkat/Documents/Coursera/Cryptography-I/6 - 1 - Introduction (11 min).mp4")) should be ("5b96aece304a1422224f9a41b228416028f9ba26b0d1058f400200f06a589949")
    }
}
