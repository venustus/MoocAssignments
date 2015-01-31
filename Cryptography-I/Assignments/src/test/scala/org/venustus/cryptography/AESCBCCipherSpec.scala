package org.venustus.cryptography

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
 * Created by venkat on 31/01/15.
 */
class AESCBCCipherSpec extends FlatSpec with Matchers {
    private val aesKey = Array.fill[Byte](16)(0)
    Random.nextBytes(aesKey)

    "AES Cipher encryption with CBC scheme" should "encrypt correctly for a message whose length is a multiple of block size" in {
        val msg = Array.fill[Byte](16) { 0.toByte }
        val myCipherImpl = new AESCBCCipher()
        myCipherImpl decrypt(myCipherImpl encrypt(msg, aesKey), aesKey) should be (msg)
    }

    "AES Cipher encryption with CBC scheme" should "encrypt correctly for a message whose length is not a multiple of block size" in {
        val msg = Array.fill[Byte](13) { 0.toByte }
        val myCipherImpl = new AESCBCCipher()
        myCipherImpl decrypt(myCipherImpl encrypt(msg, aesKey), aesKey) should be (msg)
    }

    "AES Cipher encryption with CBC scheme" should "encrypt correctly for random message" in {
        val msg = new Array[Byte](16)
        Random.nextBytes(msg)
        val myCipherImpl = new AESCBCCipher()
        myCipherImpl decrypt(myCipherImpl encrypt(msg, aesKey), aesKey) should be (msg)
    }
}
