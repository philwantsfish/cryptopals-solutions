package Set3

import java.nio.ByteBuffer
import Set2.Exercise10._

object Exercise18 {
  def encryptCTR(data: Seq[Byte], nonce: Seq[Byte], key: Seq[Byte]): Seq[Byte] = {
    // Must operate on blocks the size of the key
    val blocksize = key.size
    require(nonce.length == blocksize/2)

    val blocks = data.grouped(blocksize).toSeq
    val plaintext = blocks.zipWithIndex.flatMap { case (block, count) =>
      val countSeq = ByteBuffer.allocate(4).putInt(count).array().toSeq.reverse ++ Seq.fill(4)(0.toByte)
      val intermediate = encryptECB(nonce ++ countSeq, key, "AES/ECB/NoPadding")
      xor(intermediate, block)
    }

    plaintext
  }

  // encrypt and decrypt are the same operation
  def decryptCTR(data: Seq[Byte], nonce: Seq[Byte], key: Seq[Byte]): Seq[Byte] = encryptCTR(data, nonce, key)
}

