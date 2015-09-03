package Set2

import Exercise15._
import org.junit.Test

class Exercise15Tests {
  val blocksize = 16

  @Test
  def testMatasano1() = {
    val testString = "ICE ICE BABY\u0004\u0004\u0004\u0004".getBytes
    assert(isValidPadding(testString, blocksize))
  }

  @Test
  def testMatasano2() = {
    val testString = "ICE ICE BABY\u0005\u0005\u0005\u0005".getBytes
    assert(!isValidPadding(testString, blocksize))
  }

  @Test
  def testMatasano13() = {
    val testString = "ICE ICE BABY\u0001\u0002\u0003\u0004".getBytes
    assert(!isValidPadding(testString, blocksize))
  }
}
