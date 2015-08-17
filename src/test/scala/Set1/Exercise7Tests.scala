package Set1

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import Util.Util._
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Exercise7._
import scala.io.Source
import java.util.Base64


class Exercise7Tests extends JUnitSuite {
  @Test
  def testMatasano = {
    val base64data : Array[Byte] = Source.fromURL(getClass.getResource("/Exercise7TestData.txt")).getLines().mkString.getBytes
    val data : Array[Byte] = Base64.getDecoder.decode(base64data)
    val key = "YELLOW SUBMARINE".getBytes
    val decryptedText : Array[Byte] = decryptAESECB(data, key)
    assert(toASCIIString(decryptedText) == Exercise6Tests.matasanoAnswer)
  }




}
