package org.venustus.cryptography.authenticatedencryption

import dispatch._, Defaults._
import org.venustus.cryptography.utils.Utils

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.Breaks

/**
 * Created by venkat on 16/02/15.
 */
object PaddingOracle {

    val oracle = "http://crypto-class.appspot.com/po?er="
    val blockSize = 16

    def getResponseCode(u: String): Int = {
        val svc = url(u)
        val responseFuture: Future[Either[Throwable, String]] = Http(svc OK as.String).either
        val response = responseFuture()
        response match {
            case Right(res) => 200
            case Left(StatusCode(x)) => x
        }
    }

    def decipherUsingOracle(cipher: String): String = {
        val cipherBytes = Utils.Hex.toByteArray(cipher)
        val numBlocks = cipherBytes.length / blockSize
        val plainText = Array.fill[Byte](cipherBytes.length) { 0 }

        for {
            i <- Range(numBlocks - 1, 0, -1)
            j <- Range(blockSize - 1, 0, -1)
            k = i * blockSize + j
        } {
            val loop = new Breaks
            loop.breakable {
                for (g <- Range(0, 256)) {
                    println(s"Guess for location $j in block $i is " + ("%02x" format (g.toByte)))
                    val cipherCopy: Array[Byte] = Array.fill[Byte](cipherBytes.length) {
                        0.toByte
                    }
                    Array.copy(cipherBytes, 0, cipherCopy, 0, cipherBytes.length)
                    val paddingLength = (i + 1) * blockSize - k
                    println(s"Padding length is $paddingLength")
                    cipherCopy(k - blockSize) = (cipherCopy(k - blockSize) ^ g.toByte ^ paddingLength.toByte).toByte
                    for (l <- Range(k + 1, (i + 1) * blockSize)) {
                        cipherCopy(l - blockSize) = (cipherCopy(l - blockSize) ^ plainText(l) ^ paddingLength.toByte).toByte
                    }
                    val oracleResponse: Int = getResponseCode(oracle + Utils.Hex.valueOf(cipherCopy take ((i + 1) * blockSize)))
                    if (oracleResponse == 404) {
                        println(s"Guess is right!!")
                        plainText(k) = g.toByte
                        loop.break
                    }
                }
                println("Guesses fell through, setting to known value 9")
                plainText(k) = 0x09
            }
        }

        Utils.Hex.valueOf(plainText drop (blockSize))
    }

}
