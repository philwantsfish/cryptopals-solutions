package Set3

import org.scalatest.{FlatSpec, Matchers}
import Set1.Exercise1._
import Set3.Exercise18._
import Util.Util._

class Exercise18Tests extends FlatSpec with Matchers {
  val teststring = "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

  "CTR mode" should "decrypt the test string" in {
    val data = java.util.Base64.getDecoder.decode(teststring).toSeq
    val key = "YELLOW SUBMARINE".getBytes
    val nonce = Seq.fill(8)(0.toByte)
    val p = Exercise18.encryptCTR(data, nonce, key)
    toASCIIString(p.toArray) shouldBe "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "
  }

  it should "encrypt and decrypt as the same operation" in {
    val data = java.util.Base64.getDecoder.decode(teststring).toSeq
    val key = "YELLOW SUBMARINE".getBytes
    val nonce = Seq.fill(8)(0.toByte)
    val p = encryptCTR(data, nonce, key)
    val c = decryptCTR(p, nonce, key)
    data shouldBe c
  }
}
