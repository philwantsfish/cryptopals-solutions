package Set1

import Exercise2._
import Util.Util._

object Exercise3 {
  def singleByteXor(hexString : String, key : String) : String = {
    val fullKey = key * (hexString.length / 2)
    xor(hexString, fullKey)
  }

  def singleByteXor(hexString : String, key : Byte) : String = {
    val fullKey = Array.fill[Byte](hexString.length/2)(key)
    xor(hexString, toHexString(fullKey))
  }

  def getAllPossibleMessages(cipherText: String) : Array[String] = (0x00 to 0xFF).map{ key => singleByteXor(cipherText, key.toByte) }.toArray.map{a => toASCIIString(hexStringToByteArray(a))}
  def scoreMessageWithFrequnecyAnalysis(message : String) : Double = { message.filter{c => (c > 'a' && c < 'z') || (c > 'A' && c < 'Z') || (c == ' ')}.map{c => letterFrequencyMap(c.toLower)}.foldLeft(0.0)(_ + _) }
  def scoreMessagesWithFrequencyAnalysis(messages: Array[String]) = messages.map{ m => (scoreMessageWithFrequnecyAnalysis(m), m) }.sortWith(_._1 > _._1)

  def decryptSingleByteXor(cipherText: String) : String = {scoreMessagesWithFrequencyAnalysis(getAllPossibleMessages(cipherText))(0)._2}

  def decryptSingleByteXorTopThree(cipherText: String) : Array[String] = {
    val allMessages = scoreMessagesWithFrequencyAnalysis(getAllPossibleMessages(cipherText))

    Array(allMessages(0)._2, allMessages(1)._2, allMessages(2)._2)
  }


  // Taken from https://en.wikipedia.org/wiki/Letter_frequency , frequencies rounded to 2 scientific digits
  val letters : String = " etaoinshrdlcumwfgypbvkjxqz"
  val frequencies : Array[Double] = Array(14.0, 12.7, 9.1, 8.2, 7.0, 6.7, 6.3, 6.1, 6.0, 4.3, 4.0, 2.8, 2.8, 2.4, 2.4, 2.2, 2.0, 2.0, 1.9, 1.5, 1.0, 0.77, 0.15, 0.15, 0.095, 0.074)
  val letterFrequencyMap : Map[Char, Double] = (letters.toCharArray zip frequencies).toMap
}
