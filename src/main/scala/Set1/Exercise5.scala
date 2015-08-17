package Set1

import Util.Util._

object Exercise5 {
  // plaintext and key are Hex strings strings
  def repeatingKeyXor(data: String, key: String) : String = {
    val plaintextBytes : Array[Byte] = hexStringToByteArray(data)
    val keyBytes       : Array[Byte] = hexStringToByteArray(key)
    val xorByteArray   : Array[Byte] = plaintextBytes.grouped(keyBytes.length).flatMap{ plaintextGroup =>
      (plaintextGroup zip keyBytes).map { case(p,k) => (p^k).toByte }
    }.toArray
    toHexString(xorByteArray)
  }

}
