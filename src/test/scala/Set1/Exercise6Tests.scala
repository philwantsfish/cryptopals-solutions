package Set1

import java.nio.charset.StandardCharsets
import Set1.Exercise3._
import org.junit.Test
import Exercise6._
import Util.Util._
import org.scalatest.junit.JUnitSuite
import scala.io.Source

class Exercise6Tests extends JUnitSuite {
  @Test
  def testHammingWeight = {
    val b = 1.toByte
    assert(1 == hammingWeight(b), s"1 did not equal ${hammingWeight(b)}")

    val b2 = 7.toByte
    assert(3 == hammingWeight(b2), s"3 did not equal ${hammingWeight(b2)}")

    val b3 : Byte = (0xff).toByte
    assert(8 == hammingWeight(b3), s"8 did not equal ${hammingWeight(b3)}")
  }

  @Test
  def testHammingDistanceZero = {
    val str1 = "this"
    val str2 = "this"
    val correctAnswer = 0
    val myAnswer = hammingDistanceASCII(str1, str2)
    assert(myAnswer == correctAnswer, s"The correct hamming distance is ${correctAnswer}, but you calculated ${myAnswer}")
  }

  @Test
  def testHammingDistance = {
    val str1 = "thisB"
    val str2 = "thisA"
    val correctAnswer = 2
    val myAnswer = hammingDistanceASCII(str1, str2)
    assert(myAnswer == correctAnswer, s"The correct hamming distance is ${correctAnswer}, but you calculated ${myAnswer}")
  }

  @Test
  def testMatasanoIntroProblem = {
    val str1 = "this is a test"
    val str2 = "wokka wokka!!!"
    val correctAnswer = 37
    val myAnswer = hammingDistanceASCII(str1, str2)
    assert(myAnswer == correctAnswer, s"The correct hamming distance is ${correctAnswer}, but you calculated ${myAnswer}")
  }

  @Test
  def testProbableKeySize = {
    val encodedBase64 = Source.fromURL(getClass.getResource("/Exercise6TestData.txt")).getLines.mkString
    val ciphertextBytes : Array[Byte] = java.util.Base64.getDecoder().decode(encodedBase64)

    val keySize = getProbableKeySize(ciphertextBytes, 40)
    assert(keySize == 29, s"Detecting probable key size of ${keySize} but should be 29")
  }

  @Test
  def testSubCiphertexts = {
    val c = Array(0x00.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte, 0x04.toByte, 0x05.toByte, 0x06.toByte, 0x07.toByte)
    val keySize = 3
    val s = subCiphertexts(c, keySize)

    val a0 = Array(0.toByte, 3.toByte, 6.toByte)
    val a1 = Array(1.toByte, 4.toByte, 7.toByte)
    val a2 = Array(2.toByte, 5.toByte)

    assert(s(0) === a0)
    assert(s(1) === a1)
    assert(s(2) === a2)
  }

  @Test
  def testMatasano = {
    val encodedBase64 = Source.fromURL(getClass.getResource("/Exercise6TestData.txt")).getLines.mkString
    val ciphertextBytes : Array[Byte] = java.util.Base64.getDecoder().decode(encodedBase64)
    val keySize = getProbableKeySize(ciphertextBytes, 40)
    val eachSingleByteCiphertext : Array[Array[Byte]] = subCiphertexts(ciphertextBytes, keySize)

    // TODO: Below is the first failure. Mutable data...
    val jumbledAnswers : List[String] = eachSingleByteCiphertext.map { c => decryptSingleByteXor(toHexString(c)) }.toList
    var answer : String = ""
    for(i <- 0 until 100){
      jumbledAnswers.foreach(line => if(i < line.length) {answer += line(i)})
      answer.mkString
    }
    assert(matasanoAnswer==answer)
  }



  // Some string hackery here with the replaceAll method calls.
  // The first one replaces windows \r\n with unix \n to normalize the line endings
  // The second replaceAll is because the answer has spaces at the end of each line, so replace each newline with space newline
  // The third replaceAll is because lines that have no text do not end in a space
  val matasanoAnswer : String = """I'm back and I'm ringin' the bell
                                  |A rockin' on the mike while the fly girls yell
                                  |In ecstasy in the back of me
                                  |Well that's my DJ Deshay cuttin' all them Z's
                                  |Hittin' hard and the girlies goin' crazy
                                  |Vanilla's on the mike, man I'm not lazy.
                                  |
                                  |I'm lettin' my drug kick in
                                  |It controls my mouth and I begin
                                  |To just let it flow, let my concepts go
                                  |My posse's to the side yellin', Go Vanilla Go!
                                  |
                                  |Smooth 'cause that's the way I will be
                                  |And if you don't give a damn, then
                                  |Why you starin' at me
                                  |So get off 'cause I control the stage
                                  |There's no dissin' allowed
                                  |I'm in my own phase
                                  |The girlies sa y they love me and that is ok
                                  |And I can dance better than any kid n' play
                                  |
                                  |Stage 2 -- Yea the one ya' wanna listen to
                                  |It's off my head so let the beat play through
                                  |So I can funk it up and make it sound good
                                  |1-2-3 Yo -- Knock on some wood
                                  |For good luck, I like my rhymes atrocious
                                  |Supercalafragilisticexpialidocious
                                  |I'm an effect and that you can bet
                                  |I can take a fly girl and make her wet.
                                  |
                                  |I'm like Samson -- Samson to Delilah
                                  |There's no denyin', You can try to hang
                                  |But you'll keep tryin' to get my style
                                  |Over and over, practice makes perfect
                                  |But not if you're a loafer.
                                  |
                                  |You'll get nowhere, no place, no time, no girls
                                  |Soon -- Oh my God, homebody, you probably eat
                                  |Spaghetti with a spoon! Come on and say it!
                                  |
                                  |VIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino
                                  |Intoxicating so you stagger like a wino
                                  |So punks stop trying and girl stop cryin'
                                  |Vanilla Ice is sellin' and you people are buyin'
                                  |'Cause why the freaks are jockin' like Crazy Glue
                                  |Movin' and groovin' trying to sing along
                                  |All through the ghetto groovin' this here song
                                  |Now you're amazed by the VIP posse.
                                  |
                                  |Steppin' so hard like a German Nazi
                                  |Startled by the bases hittin' ground
                                  |There's no trippin' on mine, I'm just gettin' down
                                  |Sparkamatic, I'm hangin' tight like a fanatic
                                  |You trapped me once and I thought that
                                  |You might have it
                                  |So step down and lend me your ear
                                  |'89 in my time! You, '90 is my year.
                                  |
                                  |You're weakenin' fast, YO! and I can tell it
                                  |Your body's gettin' hot, so, so I can smell it
                                  |So don't be mad and don't be sad
                                  |'Cause the lyrics belong to ICE, You can call me Dad
                                  |You're pitchin' a fit, so step back and endure
                                  |Let the witch doctor, Ice, do the dance to cure
                                  |So come up close and don't be square
                                  |You wanna battle me -- Anytime, anywhere
                                  |
                                  |You thought that I was weak, Boy, you're dead wrong
                                  |So come on, everybody and sing this song
                                  |
                                  |Say -- Play that funky music Say, go white boy, go white boy go
                                  |play that funky music Go white boy, go white boy, go
                                  |Lay down and boogie and play that funky music till you die.
                                  |
                                  |Play that funky music Come on, Come on, let me hear
                                  |Play that funky music white boy you say it, say it
                                  |Play that funky music A little louder now
                                  |Play that funky music, white boy Come on, Come on, Come on
                                  |Play that funky music
                                  |""".stripMargin.replaceAll("\\r\\n", "\n").replaceAll("\\n", " \n").replaceAll(" \\n \\n", " \n\n")



}
