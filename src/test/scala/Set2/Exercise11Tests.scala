package Set2

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Exercise11._


class Exercise11Tests extends JUnitSuite {
  @Test
  def testGenerateKey() = {
    val size = 16
    val key : Seq[Byte] = generateRandomKey(size)
    assert(key.length == size)
  }
}
