package Set2

import java.util.Base64


import Set2.Exercise12._
import Set2.Exercise14._
import Set2.Exercise14.encryption_oracle
import Set2.Exercise14.recoverSecret
import Util.Util._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

import scala.io.Source

class Exercise14Tests extends JUnitSuite {
  @Test
  def testFindPrefixBlock() = {
    val solution = (prefix.size / blocksize) + 1
    val attempt = findPrefixBlockNumber(encryption_oracle)
    assert(attempt === solution)
  }

  @Test
  def testFindPrefixByte() = {
    val solution = blocksize - (prefix.size % blocksize)
    val attempt = findPrefixByteNumber(encryption_oracle)
    assert(attempt === solution)
  }

  @Test
  def testMatasano() = {
    val blocksize = 16
    val answerBytes = Base64.getDecoder.decode(Source.fromURL(getClass.getResource("/Exercise12TestData.txt")).getLines().mkString.getBytes)
    val b: Seq[Byte] = recoverSecret(encryption_oracle)
    assert(toHexString(answerBytes) == toHexString(b.toArray))
  }

}
