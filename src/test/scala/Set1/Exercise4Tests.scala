package Set1

import Exercise4._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class Exercise4Tests extends JUnitSuite {
  @Test
  def testMatasano = {
    val answer = decryptXorFromFile("/Exercise4TestData.txt")
    assert("Now that the party is jumping" == answer, s"You didn't decrypt properly! You decrypted the message: ${answer}")
  }
}
