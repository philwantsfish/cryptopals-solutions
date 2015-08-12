package Set1

import Exercise4._
import org.junit.Test

class Exercise4Tests {
  @Test
  def testMatasano = {
    val answer = decryptXorFromFile("/Exercise4TestData.txt")
    assert("Now that the party is jumping" == answer, s"You didn't decrypt properly! You decrypted the message: ${answer}")
  }
}
