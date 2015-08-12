package Set1

import org.junit.Test
import Exercise3._

class Exercise3Tests {
  @Test
  def testSingleByteXor = {
    val message = "2398492837987EF87333"
    val key = "41"
    val answer = singleByteXor(singleByteXor(message, key), key)
    assert( answer == message, s"Calculated ${answer} but should've calculated ${message}")
  }

  @Test
  def testScoreMessage = {
    val solution = 12.7 * 4
    val message = "eeee"
    assert(scoreMessageWithFrequnecyAnalysis(message) == solution)
  }

  @Test
  def testMatasano = {
    val cipherText = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val messages = getAllPossibleMessages(cipherText)
    val scoredMessages = scoreMessagesWithFrequencyAnalysis(messages)
    assert("Cooking MC's like a pound of bacon" == scoredMessages(0)._2, s"Found message: ${scoredMessages(0)._2}" )
  }
}
