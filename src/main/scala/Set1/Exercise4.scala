package Set1

import Set1.Exercise3._
import scala.io.Source

object Exercise4 {
  /**
   * This file attempts to decrypt single byte XOR from a file of cipher texts
   * Each cipher text is assumed to be on a newline
   * @param fileName
   * @return
   */
  def decryptXorFromFile(fileName: String) : String = {
    val cipherTextArray : Array[String] = Source.fromURL(getClass.getResource("/Exercise4TestData.txt")).getLines.toArray
    val allMessages = cipherTextArray.flatMap{ cipherText => getAllPossibleMessages(cipherText)}
    val scoredMessages = scoreMessagesWithFrequencyAnalysis(allMessages)
    scoredMessages(0)._2.trim
  }
}
