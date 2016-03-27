package org.venustus.cryptography.integrity

import java.io.{File, RandomAccessFile}
import java.security.MessageDigest

/**
 * Created by venkat on 08/02/15.
 */
object StreamingIntegrity {

    val blockSize = 1024

    /**
     * Computes a running hash on a given block of bytes, after appending the hash for previous block.
     * Returns the hash of the current block appended with previous hash.
     * @param block
     * @param previousHash
     * @return
     */
    def computeHash(block: Array[Byte], previousHash: Array[Byte]): Array[Byte] = {
        val d = MessageDigest.getInstance("SHA-256");
        d digest (block ++ previousHash)
    }

    def computeStreamingHash(fileName: String): Array[Byte] = {
        val in = new RandomAccessFile(new File(fileName), "r")
        val totalLength = in.length
        val numBlocks: Long = (totalLength / blockSize) + 1
        in seek ((numBlocks - 1) * blockSize)
        val lastBlock = Array.fill((totalLength % blockSize).toInt) { 0.toByte }
        in read (lastBlock)
        val startingHash = computeHash(lastBlock, Array[Byte]())
        (startingHash /: Range(numBlocks.toInt - 1, 0, -1))((previousHash, i) => {
            in seek ((i - 1) * blockSize)
            val block = Array.fill(blockSize) { 0.toByte }
            in read (block)
            computeHash(block, previousHash)
        })
    }

}
