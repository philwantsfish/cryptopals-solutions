package Set2

import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import Exercise11._
import Exercise10._
import Exercise9._
import Util.Util._

import scala.collection.mutable.ListBuffer

object Exercise16 {
  def escapeInput(input: String): String = { input.replaceAll(";", "\";\"").replaceAll("=", "\"=\"") }
  val blocksize = 16
  val aesKey: Seq[Byte] = generateRandomBytes(blocksize)
  val key: SecretKeySpec = new SecretKeySpec(aesKey.toArray, "AES")
  val ivBytes: Seq[Byte] = Seq.fill[Byte](aesKey.length)(0.toByte)
  val iv = new IvParameterSpec(ivBytes.toArray)


  def encryptCBC(input: String) : Seq[Byte] = {
    val fulldata  = "comment1=cooking%20MCs;userdata=" + escapeInput(input) + ";comment2=%20like%20a%20pound%20of%20bacon"
    val cipher : Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, key, iv)
    cipher.doFinal(fulldata.getBytes)
  }

  def decryptCBC(data: Seq[Byte]) : Seq[Byte] = {
    val cipher : Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, key, iv)
    cipher.doFinal(data.toArray)
  }


  def decryptCBCCheck(data: Seq[Byte]) : Boolean = { decryptCBC(data).contains(";admin=true;".getBytes) }
  //def decrypt2(data: Seq[Byte]) : Seq[Byte] = { decryptCBC(data, aesKey, iv, "AES/ECB/NoPadding") }

}
