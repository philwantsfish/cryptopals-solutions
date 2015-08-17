package Set1

import org.junit.Test
import Exercise8._
import scala.io.Source
import Util.Util._

class Exercise8Tests {
  @Test
  def testDuplicateBlockCount = {
    val ciphertext = "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"
    val keySize = 16
    val blockCount = 4
    assert(getDuplicateBlockCount(ciphertext, keySize) == blockCount)
  }

  @Test
  def testDuplicateBlockCount2 = {
    val ciphertext = "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"
    val keySize = 16
    val blockCount = 4
    val answer = getDuplicateBlockCount(ciphertext.getBytes, keySize)
    assert( answer == blockCount, s"Expected count ${blockCount} but calculated ${answer}")
  }

  @Test
  def testMatasano = {
    val ciphertexts : Array[String] = Source.fromURL(getClass.getResource("/Exercise8TestData.txt")).getLines().toArray
    assert(ciphertexts.size == 204, s"Expected  ${204} ciphertexts, but found ${ciphertexts.size}")

    val keySize = 16
    val possibleECBCiphertexts : Array[(Int, String)] = detectECB(ciphertexts, keySize)
    val answer : (Int, String) = possibleECBCiphertexts.sortWith(_._1 > _._1).head
    assert(answer._2.toLowerCase == "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a")

  }
}
