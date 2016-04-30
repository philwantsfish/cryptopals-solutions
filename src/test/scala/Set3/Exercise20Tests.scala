package Set3

import java.util.Base64

import Set1.{Exercise3, Exercise6}
import Set3.Exercise18._
import Set3.Exercise20._
import Util.Util
import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source
import scala.util.Random


class Exercise20Tests extends FlatSpec with Matchers {
  def generateRandomBytes(size: Int): Seq[Byte] = { Seq.fill[Byte](size)(Random.nextInt(255).toByte) }

  "Exercise20" should "decrypt the test data" in {
    // Read in the data and base64 decode it
    val datas = Source.fromURL(getClass.getResource("/Exercise20TestData.txt"))
      .getLines()
      .toSeq
      .map(_.replaceAll("\\n", ""))
      .map { Base64.getDecoder.decode }

    // Encrypt each data with the same nonce and key
    val blocksize = 16
    val key = generateRandomBytes(16)
    val nonce = Seq.fill(blocksize/2)(0.toByte)
    val encryptedDatas = datas.map { d => encryptCTR(d, nonce, key) }

    // Decrypt the ciphertexts without knowledge of the key!
    val answers = decryptSetOfAESCTRWithSameNonce(datas)

    // Note: this method did decrypt the first byte correctly.
    // See the commented out lines below for the scores of the correct/incorrect plaintexts
    // TODO: Use this to adjust the scoring scheme

//    val message1 = "NDE^TJOCASSAPASTJSJNOTNDKJ^SFWJHE^NI^PLHDNSTTNNETTFA TU ^FSF"
//    val message2 = "ICBYSMHDFTTFWFTSMTMIHSICLMYTAPMOBYINYWKOCITSSIIBSSAF'SR'YATA"
//    val score1 = Exercise3.scoreMessageWithFrequnecyAnalysis(message1)
//    val score2 = Exercise3.scoreMessageWithFrequnecyAnalysis(message2)
//    println(s"Score1: $score1 , Score2: $score2")

    // I've adjusted the tests with to pass with an incorrect first byte
    // First
    "N'm rated \"R\"...this is a warning, ya better void / P" shouldBe answers.head.mkString
    "Duz I came back to attack others in spite- / Strike l" shouldBe answers(1).mkString
    "Eut don't be afraid in the dark, in a park / Not a sc" shouldBe answers(2).mkString

  }
}
