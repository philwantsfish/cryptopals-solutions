package Set2

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import Exercise9._
import Util.Util._

import scala.collection.mutable.ListBuffer


object Exercise10 {
  def xor(b1: Seq[Byte], b2: Seq[Byte]) : Seq[Byte] = { (b1 zip b2).map{ case(a,b) => (a^b).toByte } }

  def encryptBlock(cipherType: String, key: Seq[Byte], data: Seq[Byte]) : Seq[Byte] = {
    if( data.length != key.length ) throw new RuntimeException(s"Key length ${key.length} and data length ${data.length} are not equal")
    val cipher : Cipher = Cipher.getInstance(cipherType)
    val keySpec : SecretKeySpec = new SecretKeySpec(key.toArray, "AES")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec)
    cipher.doFinal(data.toArray)
  }

  def decryptBlock(cipherType: String, key: Seq[Byte], data: Seq[Byte]) : Seq[Byte] = {
    if( data.length != key.length ) throw new RuntimeException(s"Key length ${key.length} and data length ${data.length} are not equal")
    val cipher : Cipher = Cipher.getInstance(cipherType)
    val keySpec : SecretKeySpec = new SecretKeySpec(key.toArray, "AES")
    cipher.init(Cipher.DECRYPT_MODE, keySpec)
    cipher.doFinal(data.toArray)
  }

  def encryptECB(data: Seq[Byte], key: Seq[Byte], cipherType: String) : Seq[Byte] = {
    val paddedData = PKCS7(data, key.size)
    paddedData.grouped(16).map{ b => encryptBlock(cipherType, key, b) }.toSeq.flatten
  }

  def decryptECB(data: Seq[Byte], key: Seq[Byte], cipherType: String) : Seq[Byte] = {
    val plaintext : Seq[Byte] = data.grouped(key.size).map{ b => decryptBlock(cipherType, key, b) }.toSeq.flatten
    removePadding(plaintext)
  }

  def encryptCBC(data: Seq[Byte], key: Seq[Byte], IV: Seq[Byte], cipherType: String) : Seq[Byte] = {
    val paddedData = PKCS7(data, 16)
    val blocks : Seq[Seq[Byte]] = paddedData.grouped(key.length).toSeq

    var previousBlock = IV
    val encryptedData = new ListBuffer[Seq[Byte]]

    blocks.foreach{ b =>
      val xorResult = xor(b, previousBlock)
      previousBlock = encryptBlock(cipherType, key, xorResult)
      encryptedData.append(previousBlock)
    }
    encryptedData.flatten.toSeq
  }

  def removePadding(data: Seq[Byte]) : Seq[Byte] = {
    val paddingAmount : Byte = data.last
    data.slice(0, data.length-paddingAmount)
  }

  // Use ciphertype AES/ECB/NoPadding, the implementation does CBC
  def decryptCBC(data: Seq[Byte], key: Seq[Byte], IV: Seq[Byte], cipherType: String) : Seq[Byte] = {
    val blocksize = 16
    val cipherBlocks : Seq[Seq[Byte]] = Seq(IV) ++ data.grouped(blocksize).toSeq.dropRight(1)
    val decryptedBlocks : Seq[Seq[Byte]] = data.grouped(blocksize).map{ b => decryptBlock(cipherType, key, b) }.toSeq
    removePadding((cipherBlocks zip decryptedBlocks).flatMap{ case(b1,b2) => xor(b1,b2)})
  }

  // Use ciphertype AES/ECB/NoPadding, the implementation does CBC
  def decryptCBCKeepPadding(data: Seq[Byte], key: Seq[Byte], IV: Seq[Byte], cipherType: String) : Seq[Byte] = {
    val blocksize = 16
    val cipherBlocks : Seq[Seq[Byte]] = Seq(IV) ++ data.grouped(blocksize).toSeq.dropRight(1)
    val decryptedBlocks : Seq[Seq[Byte]] = data.grouped(blocksize).map{ b => decryptBlock(cipherType, key, b) }.toSeq
    (cipherBlocks zip decryptedBlocks).flatMap{ case(b1,b2) => xor(b1,b2)}
  }

}
