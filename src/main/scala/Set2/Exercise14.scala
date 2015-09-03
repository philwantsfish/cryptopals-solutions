package Set2

import java.util.Base64

import Set2.Exercise10._
import Set2.Exercise11._
import Set2.Exercise12._
import scala.io.Source
import scala.util.Random
import Util.Util._


object Exercise14 {
  val blocksize = 16
  val prefix : Seq[Byte] = generateRandomBytes(Random.nextInt(255))
  def encryption_oracle(data: Seq[Byte]) : Seq[Byte] = {
    val unknownString : Seq[Byte] = Base64.getDecoder.decode(Source.fromURL(getClass.getResource("/Exercise12TestData.txt")).getLines().mkString)
    encryptECB(prefix ++ data ++ unknownString, key, "AES/ECB/NoPadding")
  }

  def findPrefixBlockNumber(func: OracleFunction): Int = {
    // the encryption oracle encrypts(randomBytes || user_input || secret)
    // make userinput very large of a single character
    // find when blocks start to duplicate
    // before the duplication can be considered the random pre-amble
    val input: Seq[Byte] = Seq.fill[Byte](blocksize*4)(0x41.toByte)
    val output = func(input).grouped(blocksize)
    output.sliding(2).indexWhere{ case Seq(o1, o2) => o1 == o2 }
  }

  def findPrefixByteNumber(func: OracleFunction): Int = {
    // Find the first block that user input beings repeating.
    // Then Find the least amount of user input to keep them repeating
    // To complete the prefix to a whole block, take the answer above and minus 2 blocksizes
    val firstBlockOfAllUserInput = findPrefixBlockNumber(func)

    val numOfBytes : Seq[Int] = (0 to blocksize*4).filter{ num =>
      val input: Seq[Byte] = Seq.fill[Byte](num)(0x41.toByte)
      val output = func(input).grouped(blocksize)
      firstBlockOfAllUserInput == output.sliding(2).indexWhere{ case Seq(o1, o2) => o1 == o2 }
    }.toSeq

    numOfBytes.sorted.head - (blocksize*2)
  }

  def recoverBlock(func: OracleFunction, blocksize: Int, previousRecoveredBytes: Seq[Byte], blockPosition: Int): Seq[Byte] = {
    var recoveredBlock : Seq[Byte] = Seq[Byte]()
    val fillerPadding = Seq.fill[Byte](findPrefixByteNumber(func))('B'.toByte)

    for(i <- 1 to blocksize){
      val fillerBytes: Seq[Byte] = fillerPadding ++ Seq.fill[Byte](blocksize-i)('A'.toByte)
      val bytesToCreateMapFrom =  fillerBytes ++ previousRecoveredBytes ++ recoveredBlock
      val map = createAnswerMap(func, blocksize, blockPosition, bytesToCreateMapFrom)
      var recoveredByte = 0x02.toByte

      val ciphertext = func(fillerBytes).slice(blocksize*blockPosition, blocksize + (blocksize*blockPosition))
      //The last block may have partial plaintext, but the block cipher will create a full block of ciphertext
      // Once we have retrieved all of the secret the map will no longer have an answer
      // Return in this case
      // TODO, i thought the padding should make the plaintext a full block. Not sure why it isnt
      val plaintextOption = map.get(ciphertext)
      plaintextOption match {
        case Some(plaintext) => recoveredByte = plaintext.last
        case None => return recoveredBlock
      }
      recoveredBlock = recoveredBlock :+ recoveredByte
    }
    recoveredBlock
  }

  def recoverSecret(func: OracleFunction): Seq[Byte] = {
    var secret : Seq[Byte] = Seq[Byte]()

    val prefixPadding : Seq[Byte] = Seq.fill[Byte](findPrefixByteNumber(func))(0x41.toByte)
    val numberOfBlocks = func(prefixPadding).grouped(blocksize).size + 1 // Add one block because we will be sending blocksize-1 'A' to decrypt
    val numberOfBlocksToDecrypt = numberOfBlocks - findPrefixBlockNumber(func)
    val startBlockNumber = findPrefixBlockNumber(func)

    for(i <- startBlockNumber to (startBlockNumber + numberOfBlocksToDecrypt)){
      secret = secret ++ recoverBlock(func, blocksize, secret, i)
    }

    removePadding(secret)
  }
}
