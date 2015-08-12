package Set1

import Util.Util._

object Exercise2 {
  def fixedXor(message1: String, message2: String) : String = {
    val byteArray1 = toByteArray(message1)
    val byteArray2 = toByteArray(message2)
    val xorByteArray = (byteArray1 zip byteArray2).map{ case(a,b) => (a^b).toByte }
    toHexString(xorByteArray)
  }
}
