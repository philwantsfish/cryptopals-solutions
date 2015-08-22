package Set1

import Util.Util._

object Exercise2 {
  def xor(message1: String, message2: String) : String = { xor(hexStringToByteArray(message1), hexStringToByteArray(message2)) }
  def xor(b1: Array[Byte], b2: Array[Byte]) : String = { toHexString((b1 zip b2).map{ case(a,b) => (a^b).toByte }) }
}
