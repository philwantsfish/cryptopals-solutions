package Set2

import java.util.Base64
import java.util.Base64.Decoder
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Set2.Exercise12._
import Util.Util._
import Set1.Exercise8._
import scala.io.Source


class Exercise12Tests extends JUnitSuite {
  @Test
  def testDetectBlockSize() = {
    assert(detectECBBlockSize(encryption_oracle) == 16)
  }

  @Test
  def testAnswerMap() = {
    val blocksize = 16
    val bytes = ("A"*(blocksize-1)).getBytes
    val map = createAnswerMap(encryption_oracle, blocksize, 0, bytes)
    val ciphertext = encryption_oracle(bytes).slice(0,blocksize)
    val attempt = map(ciphertext)
    val solution = bytes :+ 'R'.toByte
    assert(toHexString(solution) === toHexString(attempt))
  }

  @Test
  def testRecoverByte() = {
    val blocksize = 16
    val bytes = ("A"*(blocksize-1)).getBytes
    val map = createAnswerMap(encryption_oracle, blocksize, 0, bytes)
    val b: Byte = recoverSingleByte(encryption_oracle, bytes, map, blocksize, 0)
    assert(b == 'R'.toByte)
  }

  @Test
  def testRecoverBlock() = {
    val blocksize = 16
    val b: Seq[Byte] = recoverBlock(encryption_oracle, blocksize, Seq[Byte](), 0)
    assert(toASCIIString(b.toArray) == "Rollin' in my 5.")
  }

  @Test
  def testMatasano() = {
    val blocksize = 16
    val answerBytes = Base64.getDecoder.decode(Source.fromURL(getClass.getResource("/Exercise12TestData.txt")).getLines().mkString.getBytes)
    val b: Seq[Byte] = recoverSecret(encryption_oracle, blocksize)
    assert(toHexString(answerBytes) == toHexString(b.toArray))
  }
}
