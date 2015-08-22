package Set2

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Exercise9._
import Util.Util._

class Exercise9Tests extends JUnitSuite {
  @Test
  def testMatasano = {
    val key = "YELLOW SUBMARINE"
    val blocksize = 20
    val solution : Seq[Byte] = "YELLOW SUBMARINE\u0004\u0004\u0004\u0004".getBytes
    val attempt : Seq[Byte] = PKCS7(key, blocksize)
    assert(solution == attempt, s"Solution: $solution, atttempt: $attempt")
  }

  @Test
  def testPKCS7 = {
    val key = "YELLOW SUBMARI"
    val blocksize = 20
    val solution : Seq[Byte] = "YELLOW SUBMARI\u0006\u0006\u0006\u0006\u0006\u0006".getBytes
    val attempt : Seq[Byte] = PKCS7(key, blocksize)
    assert(solution == attempt, s"Solution: $solution, atttempt: $attempt")
  }

  @Test
  def testPKCS7longinput = {
    val data = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that.".getBytes
    val solution = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that.\u0007\u0007\u0007\u0007\u0007\u0007\u0007".getBytes
    val attempt = PKCS7(data, 16)
    assert(toHexString(attempt.toArray) == toHexString(solution))
  }
}
