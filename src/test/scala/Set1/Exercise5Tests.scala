package Set1

import java.nio.charset.StandardCharsets
import Exercise5._
import org.junit.Test
import Util.Util._
import org.scalatest.junit.JUnitSuite

class Exercise5Tests extends JUnitSuite {
  @Test
  def testSameSizeKey = {
    val plaintext = "000000"
    val key = "010101"
    val ciphertext = repeatingKeyXor(plaintext, key)
    assert(ciphertext == key)
  }

  @Test
  def testDifferentSizeKey = {
    val plaintext = "000000"
    val key = "0F0E"
    val ciphertext = repeatingKeyXor(plaintext, key)
    assert(ciphertext == "0F0E0F")
  }

  @Test
  def testMatasano = {
    val plaintext = toHexString("Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal".getBytes(StandardCharsets.US_ASCII))
    val answer = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    val key = toHexString("ICE".getBytes(StandardCharsets.US_ASCII))

    val ciphertext = repeatingKeyXor(plaintext, key).toLowerCase()
    assert(answer == ciphertext, s"Did not encrypt properly, you encrypted it as ${ciphertext}")
  }
}
