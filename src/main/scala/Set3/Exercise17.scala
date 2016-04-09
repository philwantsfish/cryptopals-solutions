package Set3

import Set2.Exercise10._
import Set2.Exercise15
import Util.Util._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Exercise17 {
  val blockSize = 16
  val key = Seq.fill[Byte](blockSize)(Random.nextInt(255).toByte)
  val IV = Seq.fill[Byte](key.length)(Random.nextInt(255).toByte)

  def encryptAndGetIV(data: Seq[Byte]): (Seq[Byte], Seq[Byte]) = {
    (encryptCBC(data, key, IV, "AES/ECB/NoPadding"), IV)
  }

  def decryptPaddingCheck(ciphertext: Seq[Byte]): Boolean = {
    val plaintext: Seq[Byte] = decryptCBCKeepPadding(ciphertext, key, IV, "AES/ECB/NoPadding")
    Exercise15.isValidPadding(plaintext, blockSize)
  }

  def decryptUsingPaddingOracle(ciphertext: Seq[Byte]): Seq[Byte] = {
    val numberOfBlocks = ciphertext.grouped(16).length

    // The first block of ciphertext is decrypted using the IV. Lets prepend the IV and stop when "decrypting" the iv block
    val ivciphertext = IV ++ ciphertext

    // decrypt one block at a time
    val z = (0 to numberOfBlocks-1).flatMap { jjj =>
      // on each iteration we need to keep track of th known intermediate bytes and use them to correc the padding
      var bytesToCorrectPadding3: ArrayBuffer[Byte] = ArrayBuffer()

      // on each iteration remove the block we have decrypted and start on the next
      val ciphertext3 = ivciphertext.slice(0, ivciphertext.length - (jjj*blockSize))

      // decrypt one character at a time
      (1 to blockSize).map { i =>
        val ciphertextguessbyte = (0 to 255).filter { guessbyte =>
          val guessblock = (Seq.fill[Byte](blockSize - i)(0.toByte) :+ guessbyte.toByte) ++ bytesToCorrectPadding3.map(b => (b ^ i).toByte)
          val guessciphertext = ciphertext3.slice(0, blockSize * (numberOfBlocks - 2)) ++ guessblock ++ ciphertext3.slice(ciphertext3.length - blockSize, ciphertext3.length)
          decryptPaddingCheck(guessciphertext)
        }.map(_.toByte).head

        val intermediatelastbyte = ciphertextguessbyte ^ i
        val actualCiphertextByte = ciphertext3(ciphertext3.length - blockSize - i)
        val plaintextlastbyte = (intermediatelastbyte ^ actualCiphertextByte).toByte

        bytesToCorrectPadding3 = bytesToCorrectPadding3.+:(intermediatelastbyte.toByte)
        plaintextlastbyte
      }
    }.toSeq

    removePadding(z.reverse)
  }

}
