package Set3

import java.util.Base64

import Set2.Exercise10._
import Set2.Exercise9.PKCS7
import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random
import Exercise17._
import scala.io.Source
import Util.Util._


class Exercise17Tests extends FlatSpec with Matchers {
  class Test {
    val dataListBase64 = Source.fromURL(getClass.getResource("/Exercise17TestData.txt")).getLines().toSeq.map(_.replaceAll("\\n", ""))
    val dataList = dataListBase64.map(Base64.getDecoder.decode)
    val data = dataList(Random.nextInt(dataList.length))
  }

  "encryptAndGetKeyAndIV" should "encrypt the data properly" in new Test() {
    val (ciphertext, iv) = encryptAndGetIV(data)
    data shouldBe decryptCBC(ciphertext, Exercise17.key, iv, "AES/ECB/NoPadding")
  }

  "decryptFirstBlock" should "decrypt the first block of ciphertext" in new Test() {
//    println(s"Phil: data is: ${toASCIIString(data)}")
    val (ciphertext, iv) = encryptAndGetIV(data)
    val decryptedData = decryptUsingPaddingOracle(ciphertext)
    data shouldBe decryptedData
  }
}
