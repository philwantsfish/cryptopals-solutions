package Set2

import Set2.Exercise10._
import Set2.Exercise11._
import scala.io.Source
import scala.util.Random
import java.util.Base64
import Util.Util._
import Set1.Exercise8._

object Exercise12 {
  type OracleFunction = (Seq[Byte] => Seq[Byte])
  val key = generateRandomBytes(16)

  def encryption_oracle(data: Seq[Byte]) : Seq[Byte] = {
    val unknownString : Seq[Byte] = Base64.getDecoder.decode(Source.fromURL(getClass.getResource("/Exercise12TestData.txt")).getLines().mkString)
    encryptECB(data ++ unknownString, key, "AES/ECB/NoPadding")
  }

  def isAllBlocksEqual(blocks: Seq[Seq[Byte]]) : Boolean = { !blocks.exists(b => b != blocks.head) }

  def detectECBBlockSize(func: OracleFunction) : Int = {
    def recurse(keySize: Int , inputSize: Int) : Int = {
      val twoBlocks = func(("A"*inputSize).getBytes).grouped(keySize).toSeq.slice(0,2)
      val fourBlocks = func(("A"*inputSize*2).getBytes).grouped(keySize).toSeq.slice(0,4)
      if(isAllBlocksEqual(twoBlocks) && isAllBlocksEqual(fourBlocks)) { keySize }
      else recurse(keySize+1, inputSize+2)
    }
    recurse(1,2)
  }

  def createAnswerMap(func: OracleFunction, blocksize: Int, blockPosition:Int, knownBytes: Seq[Byte]): Map[Seq[Byte], Seq[Byte]] = {
    if(knownBytes.length % blocksize == 1) throw new RuntimeException(s"Wrong number of known bytes to create map, should be ${blocksize-1} but got ${knownBytes.length}")
    val knownPlaintexts: Seq[Seq[Byte]] = Seq.range(0, 256).map{v => knownBytes :+ v.toByte }
    val ciphertexts    : Seq[Seq[Byte]] = knownPlaintexts.map{p => func(p).slice(blocksize*blockPosition, blocksize + (blocksize*blockPosition))}
    (ciphertexts zip knownPlaintexts).toMap
  }


  def recoverSecret(func: OracleFunction, blocksize: Int): Seq[Byte] = {
    var secret : Seq[Byte] = Seq[Byte]()
    val numberOfBlocks = func("".getBytes).length/blocksize

    for(i <- 0 to numberOfBlocks-1){
      secret = secret ++ recoverBlock(func, blocksize, secret, i)
    }

    removePadding(secret)
  }

  def recoverBlock(func: OracleFunction, blocksize: Int, previousRecoveredBytes: Seq[Byte], blockPosition: Int): Seq[Byte] = {
    var recoveredBlock : Seq[Byte] = Seq[Byte]()

    for(i <- 1 to blocksize){
      val fillerBytes: Seq[Byte] = Seq.fill[Byte](blocksize-i)('A'.toByte)
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
}
