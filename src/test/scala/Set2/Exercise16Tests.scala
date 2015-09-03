package Set2

import Exercise16._
import org.junit.Test
import Util.Util._

class Exercise16Tests {
  @Test
  def testFunctions() = {
    val encryptedData = encryptCBC("Some random data")
    assert(!decryptCBCCheck(encryptedData))
  }

  @Test
  def testMatasano() = {
    val inputString = "AadminAtrueA"
    val encryptedData: Seq[Byte] = encryptCBC(inputString)
    var modifiedEncryptedData: Seq[Byte] = encryptedData.updated(16, (encryptedData(16) ^ 0x7A.toByte).toByte)
    modifiedEncryptedData = modifiedEncryptedData.updated(27, (encryptedData(27) ^ 0x7A.toByte).toByte)
    modifiedEncryptedData = modifiedEncryptedData.updated(22, (encryptedData(22) ^ 0x7C.toByte).toByte)

    val unencrypted = decryptCBC(modifiedEncryptedData)

    //println(toASCIIString(unencrypted.toArray))
    assert(unencrypted.containsSlice(";admin=true;".getBytes))
  }
}
