package Set2

import javax.crypto.spec.SecretKeySpec
import Exercise10._
import Set1.Exercise8._
import scala.util.Random

object Exercise11 {
  def generateRandomBytes(size: Int) : Seq[Byte] = { Seq.fill[Byte](size)(Random.nextInt(255).toByte) }
  def generateAESKey() : SecretKeySpec = { new SecretKeySpec(generateRandomBytes(16).toArray, "AES") }

  def encryption_oracle(data: Seq[Byte], keySize: Int) : (String, Seq[Byte]) = {
    val plaintext : Seq[Byte] = generateRandomBytes(Random.nextInt(5)+5) ++ data ++ generateRandomBytes(Random.nextInt(5)+5)
    val encryptionModeECB = Random.nextBoolean()
    if(encryptionModeECB) ("ECB", encryptECB(plaintext, generateRandomBytes(keySize), "AES/ECB/NoPadding"))
    else ("CBC", encryptCBC(plaintext, generateRandomBytes(keySize), generateRandomBytes(keySize), "AES/CBC/NoPadding"))
  }

  def detectEncryptionMode(data: Seq[Byte], keySize: Int) : String = {
    if(getDuplicateBlockCount(data.toArray, keySize) > 1) "ECB"
    else "CBC"
  }

}
