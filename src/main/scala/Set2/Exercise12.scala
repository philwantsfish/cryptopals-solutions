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
    if(knownBytes.length % blocksize == 1) throw new RuntimeException(s"Wrong number of known bytes, should be ${blocksize-1} but got ${knownBytes.length}")
    val knownPlaintexts: Seq[Seq[Byte]] = Seq.range(0, 256).map{v => knownBytes :+ v.toByte }
    val ciphertexts: Seq[Seq[Byte]] = knownPlaintexts.map{p => func(p).slice(blocksize*blockPosition, blocksize + (blocksize*blockPosition))}
    val answerMap: Map[Seq[Byte], Seq[Byte]] = (ciphertexts zip knownPlaintexts).toMap
    answerMap
  }

  def recoverSecret(func: OracleFunction, blocksize: Int): Seq[Byte] = {
    var secret : Seq[Byte] = Seq[Byte]()
    val secretSize = func("".getBytes).length
    val numberOfBlocks = secretSize/16

    
    for(i <- 0 to numberOfBlocks-1){
      secret = secret ++ recoverBlock(func, blocksize, secret, i)
    }

    removePadding(secret)
  }

  def recoverBlock(func: OracleFunction, blocksize: Int, previousRecoveredBytes: Seq[Byte], blockPosition: Int): Seq[Byte] = {
    var recoveredBlock : Seq[Byte] = Seq[Byte]()

    for(i <- 0 to blocksize-1){
      val fillerBytes: Seq[Byte] = ("A"*(blocksize-1-i)).getBytes
      val bytesToCreateMapFrom =  fillerBytes ++ previousRecoveredBytes ++ recoveredBlock
      val map = createAnswerMap(func, blocksize, blockPosition, bytesToCreateMapFrom)
      var recoveredByte = 0x00.toByte
      try {
        recoveredByte = recoverSingleByte(func, fillerBytes, map, blocksize, blockPosition)
      } catch {
        case e: Exception => return recoveredBlock
      }
      recoveredBlock = recoveredBlock :+ recoveredByte
    }
    recoveredBlock
  }


  def recoverSingleByte(func: OracleFunction, fillerBytes: Seq[Byte], map: Map[Seq[Byte], Seq[Byte]], blocksize: Int, blockPosition: Int): Byte = {
    val ciphertext = func(fillerBytes).slice(blocksize*blockPosition, blocksize + (blocksize*blockPosition))
    val plaintextBlock = map(ciphertext)
    plaintextBlock.last
  }
}
