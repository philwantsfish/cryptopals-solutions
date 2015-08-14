package Set1

import org.junit.Test
import org.scalatest.junit.JUnitSuite

class Exercise1Tests extends JUnitSuite {
  @Test
  def testCharMap = {
    val testCases = Map(0 -> 'A', 21 -> 'V', 60 -> '8')
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64char(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testBase64 = {
    val testCases = Map("AAA" -> "QUFB")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testBase64Padding = {
    val testCases = Map("AAAA" -> "QUFBQQ==")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testBase64Padding2 = {
    val testCases = Map("AAAAA" -> "QUFBQUE==")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testHexBase64 = {
    val testCases = Map("414141" -> "QUFB")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64Hex(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testHexBase64Padding = {
    val testCases = Map("41414141" -> "QUFBQQ==")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64Hex(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testHexBase64Padding2 = {
    val testCases = Map("4141414141" -> "QUFBQUE==")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64Hex(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }

  @Test
  def testMatasano = {
    val testCases = Map("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" -> "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
    for ((key, solution) <- testCases) {
      assert(Exercise1.base64Hex(key) == solution, s"Key ${key} did not match ${solution}")
    }
  }
}
