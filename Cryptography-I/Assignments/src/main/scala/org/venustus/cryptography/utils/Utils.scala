package org.venustus.cryptography.utils

/**
 * Created by venkat on 09/02/15.
 */
object Utils {
    object Hex {
        def valueOf(buf: Array[Byte]): String = buf.map("%02x" format _).mkString

        def toByteArray(s: String) = {
            val len = s.length();
            val data = new Array[Byte](len / 2)
            var i = 0

            while (i < len) {
                val b = (Character.digit(s.charAt(i), 16) << 4) +
                    (Character.digit(s.charAt(i + 1), 16))

                data(i / 2) = b.asInstanceOf[Byte]

                i += 2
            }
            data
        }
    }
}
