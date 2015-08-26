package Set2

import Exercise13._
import Set2.Exercise10._
import Set2.Exercise11._
import Set2.Exercise11.encryption_oracle
import Set2.Exercise12._
import org.junit.Test
import Util.Util._

class Exercise13Tests {
  val blocksize = 16
  val tmpAESKey : Seq[Byte] = generateRandomBytes(blocksize)

  @Test
  def testParsingRoutine() = {
    val input = "foo=bar&baz=qux&zap=zazzle"
    val solution = "{\n  foo: 'bar',\n  baz: 'qux',\n  zap: 'zazzle'\n}"
    val attempt = parsingRoutine(input)
    assert(attempt == solution, s"Expected $solution, but got \n$attempt")

  }

  @Test
  def testProfileFor() = {
    val input = "test@test.com"
    val solution = "email=test@test.com&uid=10&role=user"
    val attempt = profile_for(input)
    assert(attempt == solution, s"Expected $solution, but got \n$attempt")
  }

  @Test
  def testProfileForWithMetaChars() = {
    val input = "test@test.com&role=admin"
    val solution = "email=test@test.comroleadmin&uid=10&role=user"
    val attempt = profile_for(input)
    assert(attempt == solution, s"Expected $solution, but got \n$attempt")
  }

  @Test
  def testMatasanoSetupFunctions() = {
    val profile : Seq[Byte] = profile_for("phil@philwants.fish").getBytes

    val encryptedProfile : Seq[Byte] = encryptECB(profile, tmpAESKey, "AES/ECB/NoPadding")
    val decryptedProfile = decryptECB(encryptedProfile, tmpAESKey, "AES/ECB/NoPadding")

    val decryptedProfilePlain = toASCIIString(decryptedProfile.toArray)
    assert(decryptedProfilePlain == "email=phil@philwants.fish&uid=10&role=user")

    val parsedAttempt = parsingRoutine(decryptedProfilePlain)
    val solution = "{\n  email: 'phil@philwants.fish',\n  uid: 10,\n  role: 'user'\n}"
    assert( parsedAttempt == solution, s"Expected $solution but got $parsedAttempt" )
  }

  @Test
  def testMatasano() = {
    // Ending profile will be
    val solution = "email=phil@fish.com&uid=10&role=admin"

    // We know the last block must contain admin, the character before admin is = which is filtered
    // therefore the last block must be admin + padding bytes
    // Given this, the second block must contain the 13 bytes &uid=10&role=
    // Which leaves us 13 bytes for the email string. 
    // b1: email=phil@fish.
    // b2: com&uid=10&role=
    // b3: admin\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b

    val block1and2 = encryptECB(profile_for("phil@fish.com").getBytes, tmpAESKey, "AES/ECB/NoPadding").slice(0,blocksize*2)
    val answerBlock1and2 = encryptECB("email=phil@fish.com&uid=10&role=".getBytes, tmpAESKey, "AES/ECB/NoPadding").slice(0, blocksize*2)
    assert(toHexString(block1and2) == toHexString(answerBlock1and2), s"Expected ${toHexString(answerBlock1and2)} but got ${toHexString(block1and2)}")

    val block3 = encryptECB(profile_for("FILLERRRRRadmin\u000b\u000b\u000b\u000b\u000b\u000b\u000b\u000b\u000b\u000b\u000b").getBytes, tmpAESKey, "AES/ECB/NoPadding").slice(blocksize, blocksize*2)

    val plaintext = toASCIIString(decryptECB(block1and2 ++ block3, tmpAESKey, "AES/ECB/NoPadding").toArray)
    assert(plaintext == solution)
  }

}
