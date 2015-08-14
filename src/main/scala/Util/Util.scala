package Util

object Util {
  def toByteArray(hexString : String) : Array[Byte] = {
    if (hexString.contains("[^0-9A-Fa-f]") || hexString.length % 2 != 0) throw new Exception(s"Not a hex string: ${hexString}")
    hexString.grouped(2).map(Integer.parseInt(_,16).toByte).toArray
  }

  def toHexString(byteArray : Array[Byte]) : String = { byteArray.map("%02X".format(_)).mkString }
  def toASCIIString(byteArray : Array[Byte]) : String = { new String(byteArray, "ASCII") }

  /*def isPrintableASCIIChar(b : Byte) : Boolean = { ((b&0xff) > 31 && (b&0xff) < 127) || (b&0xff)==9 || (b&0xff)==13 }
  def isASCIIString(byteArray : Array[Byte]) : Boolean = { byteArray.count(b => !isPrintableASCIIChar(b)) == 0 }
  def getASCIICharCount(message : String) : Int = { toByteArray(message).count(isPrintableASCIIChar) }*/
}
