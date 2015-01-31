package org.venustus.cryptography

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.util.Random

/**
 * Created by venkat on 31/01/15.
 */
class AESCBCCipher extends EncryptionScheme {
    private val cipherImpl = Cipher.getInstance("AES/ECB/NoPadding")
    private val blockSize = 16

    def encrypt(msg: Array[Byte], key: Array[Byte]) = {
        val iv = Array.fill[Byte](blockSize)(0)
        Random.nextBytes(iv)
        cipherImpl.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"))
        val numPaddingBytes = if(msg.length % blockSize == 0) blockSize else blockSize - (msg.length % blockSize)
        val paddedMsg: Array[Byte] = msg ++ Array.fill[Byte](numPaddingBytes) { numPaddingBytes.toByte }

        val res = ((iv, Vector[Byte]()) /: (0 until (paddedMsg.length / blockSize)))((acc: Pair[Array[Byte], Vector[Byte]], i) => {
            acc match {
                case (currentIv, result) => {
                    val mi = paddedMsg slice (i * blockSize, (i + 1) * blockSize)
                    val miXorIv: Array[Byte] = (currentIv zip mi) map { case (b1, b2) => (b1 ^ b2).toByte }
                    val ci = cipherImpl.doFinal(miXorIv)
                    (ci, result ++ ci)
                }
            }
        })
        iv ++ res._2
    }

    def decrypt(encryptedMsg: Array[Byte], key: Array[Byte]) = {
        val (iv, actualEncryptedMsg) = encryptedMsg splitAt blockSize
        cipherImpl.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"))
        val numEncryptedBlocks = actualEncryptedMsg.length / blockSize
        val (finalIv, msg) = ((iv, Vector[Byte]()) /: (0 until numEncryptedBlocks))((acc: Pair[Array[Byte], Vector[Byte]], i) => {
            acc match {
                case (currentIv, result) => {
                    val ci = actualEncryptedMsg slice (i * blockSize, (i + 1) * blockSize)
                    val mi: Array[Byte] = (cipherImpl.doFinal(ci) zip currentIv) map { case (b1, b2) => (b1 ^ b2).toByte }
                    (ci, result ++ mi)
                }
            }
        })
        (msg take (msg.size - msg.last.toInt)).toArray
    }

}
