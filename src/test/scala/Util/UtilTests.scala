package Util

import org.junit.Test
import Util._

class UtilTests {
  @Test
  def testByteToHex = {
    val testCases = Map(Array(0xFF.toByte, 0xFF.toByte) -> "FFFF", Array(0x41.toByte, 0x2F.toByte) -> "412F")
    for ((key, solution) <- testCases) {
      val answer = Util.toHexString(key)
      assert(answer == solution, s"Using key, ${key}, ${answer} did not hex string ${solution}")
    }
  }

  /*@Test
  def testGetASCIICharCount = {
    val message = "00202b2f7ffffe"
    val count = 3
    val answer = getASCIICharCount(message)
    assert( answer == count, s"Found count ${answer} but correct count is ${count}")
  }

  @Test
  def testIsASCIIString = {
    val testCases = Map("E4C8C8CCCEC9C087EAE480D487CBCECCC287C687D7C8D2C9C387C8C187C5C6C4C8C9" -> false, "4142434441424344" -> true)
    for ((key, solution) <- testCases) {
      assert(isASCIIString(toByteArray(key)) == solution, s"Key ${key} did not match ${solution}")
    }
  }*/
}
