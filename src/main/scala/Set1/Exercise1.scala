package Set1

import java.nio.charset.StandardCharsets

object Exercise1 {
  def base64(input: Array[Byte]) : String = { input.grouped(3).map{ getBase64Chars }.mkString }
  def base64(input: String) : String = { base64(input.getBytes(StandardCharsets.US_ASCII)) }
  def base64Hex(hexString: String) : String = {
    if (hexString.contains("[^0-9A-Fa-f]") || hexString.length % 2 != 0) throw new Exception(s"Not a hex string: ${hexString}")
    val byteArray = hexString.grouped(2).map(Integer.parseInt(_,16).toByte).toArray
    base64(byteArray)
  }


  private def getBase64Chars(input: Array[Byte]) : String = {
    val c1 =                      base64char( input(0) >> 2 )
    val c2 = if(1 < input.length) base64char( ((input(0) & 0x03) << 4 ) | ((input(1) & 0xF0) >> 4) )
    else                          base64char((input(0) & 0x03) << 4)
    val c3 = if(2 < input.length) base64char( ((input(1) & 0x0F) << 2 ) | ((input(2) & 0xC0) >> 6) )
    else if(1 < input.length)     base64char((input(1) & 0x0F) << 2) + "="
    else                          '='
    val c4 = if(2 < input.length) base64char( input(2) & 0x3F )
    else                          '='

    c1.toString+c2+c3+c4
  }


  def base64char(offset : Int) : Char = {
    if (offset > 63 || offset < 0) throw new IndexOutOfBoundsException(s"Not a valid base64 character: ${offset}")
    charMap(offset)
  }
  private val base64Indices : Array[Int] = (0 to 63).toArray
  private val base64Chars : Array[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray
  private val charMap : Map[Int, Char] = (base64Indices zip base64Chars).toMap
}
