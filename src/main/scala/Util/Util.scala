package Util

object Util {
  def hexStringToByteArray(hexString : String) : Array[Byte] = {
    if (hexString.contains("[^0-9A-Fa-f]") || hexString.length % 2 != 0) throw new Exception(s"Not a hex string: ${hexString}")
    hexString.grouped(2).map(Integer.parseInt(_,16).toByte).toArray
  }

  def toHexString(byteArray : Array[Byte]) : String = { byteArray.map("%02X".format(_)).mkString }
  def toASCIIString(byteArray : Array[Byte]) : String = { new String(byteArray, "ASCII") }
}
