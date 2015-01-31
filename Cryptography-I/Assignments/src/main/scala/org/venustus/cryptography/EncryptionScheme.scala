package org.venustus.cryptography

/**
 * Created by venkat on 31/01/15.
 * Generic encryption scheme that encrypts/decrypts an arbitrarily long message.
 * Supported schemes: AES with CBC, AES with CTR.
 */
trait EncryptionScheme {

    def encrypt(msg: Array[Byte], key: Array[Byte]): Array[Byte]

    def decrypt(encryptedMsg: Array[Byte], key: Array[Byte]): Array[Byte]
}
