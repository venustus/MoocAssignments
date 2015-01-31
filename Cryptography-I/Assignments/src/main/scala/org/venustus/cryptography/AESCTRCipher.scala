package org.venustus.cryptography

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.util.Random

/**
 * Created by venkat on 31/01/15.
 */
class AESCTRCipher extends EncryptionScheme {
    private val cipherImpl = Cipher.getInstance("AES/ECB/NoPadding");
    private val blockSize = 16;

    def encrypt(msg: Array[Byte], key: Array[Byte]) = {
        val iv = Array.fill[Byte](blockSize)(0)
        Random.nextBytes(iv)
        val ivInt = BigInt(iv)
        cipherImpl.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"));

        val numPaddingBytes = if(msg.length % blockSize == 0) 0 else blockSize - (msg.length % blockSize)
        val paddedMsg: Array[Byte] = if(numPaddingBytes > 0) msg ++ Array.fill[Byte](numPaddingBytes) { 0.toByte } else msg

        val res = (Vector[Byte]() /: (0 until (paddedMsg.length / blockSize)))((acc, i) => {
            val msgBlock: Array[Byte] = paddedMsg slice (i * blockSize, (i + 1) * blockSize)
            val pad: Array[Byte] = cipherImpl.doFinal((ivInt + i).toByteArray)
            val cipherBlock: Array[Byte] = (pad zip msgBlock) map { case (b1, b2) => (b1 ^ b2).toByte }
            acc ++ cipherBlock
        }).toArray

        iv ++ (res take (msg.length))
    }

    def decrypt(encryptedMsg: Array[Byte], key: Array[Byte]) = {
        val (iv, actualEncryptedMsg) = encryptedMsg splitAt blockSize
        val ivInt = BigInt(iv)
        cipherImpl.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"))

        val numPaddingBytes = if(actualEncryptedMsg.length % blockSize == 0) 0 else blockSize - (actualEncryptedMsg.length % blockSize)
        val paddedCipher: Array[Byte] = if(numPaddingBytes > 0) actualEncryptedMsg ++ Array.fill[Byte](numPaddingBytes) { 0.toByte } else actualEncryptedMsg

        val res = (Vector[Byte]() /: (0 until (paddedCipher.length / blockSize)))((acc, i) => {
            val cipherBlock: Array[Byte] = paddedCipher slice (i * blockSize, (i + 1) * blockSize)
            val pad: Array[Byte] = cipherImpl.doFinal((ivInt + i).toByteArray)
            val msgBlock: Array[Byte] = (pad zip cipherBlock) map { case (b1, b2) => (b1 ^ b2).toByte }
            acc ++ msgBlock
        }).toArray
        res take (actualEncryptedMsg.length)
    }

}
