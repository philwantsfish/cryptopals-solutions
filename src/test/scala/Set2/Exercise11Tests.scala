package Set2

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Exercise11._


class Exercise11Tests extends JUnitSuite {
  @Test
  def testGenerateKey() = {
    val size = 16
    val key : Seq[Byte] = generateRandomBytes(size)
    assert(key.length == size)
  }

  @Test
  def testMatasano() = {
    val keySize = 16
    for(i <- 1 to 10) {
      val data = ("A" * 768).getBytes
      val (mode,output) = encryption_oracle(data, keySize)
      assert(mode == detectEncryptionMode(output, keySize))
    }
  }
}
