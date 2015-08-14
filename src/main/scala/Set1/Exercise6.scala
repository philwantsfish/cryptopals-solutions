package Set1

import java.nio.charset.StandardCharsets

import Util.Util._

object Exercise6 {
  def hammingWeight(b : Byte) : Int = {
    def recurseHammingWeight(b: Byte, i: Int, sum: Int) : Int = {
      if(i == 8) sum
      else recurseHammingWeight((b>>1).toByte, i+1, sum + (b&1))
    }
    recurseHammingWeight(b, 0, 0)
  }

  def hammingDistanceASCII(str1 : String, str2: String) : Int = { hammingDistance(str1.getBytes(StandardCharsets.US_ASCII), str2.getBytes(StandardCharsets.US_ASCII)) }
  def hammingDistanceHex(str1 : String, str2: String) : Int = { hammingDistance(toByteArray(str1), toByteArray(str2)) }
  def hammingDistance(b1 : Array[Byte], b2: Array[Byte]) : Int = {
    if(b1.length != b2.length) throw new Exception(s"Need to have equal sized arrays. You passed sizes of  ${b1.length} and ${b2.length}")
    (b1 zip b2).map{case(b1,b2) => hammingWeight((b1^b2).toByte)}.sum
  }
  def normalizedHammingDistance(b1 : Array[Byte], b2: Array[Byte]) : Double = { hammingDistance(b1, b2).toDouble / b1.length.toDouble }

  def getProbableKeySize(ciphertextBytes : Array[Byte], maxKeySize: Int) : Int = {
    val keyStart = 2
    val numberOfKeyBlocks = 50
    val hammingDistances : Seq[Double] = (keyStart to maxKeySize).map{ keySize =>
      // Break the byte array up into keysize*2 sized blocks
      val blocks : Seq[Array[Byte]] = ciphertextBytes.grouped(keySize*2).toSeq.dropRight(1)

      // Get the normalized hamming distance for each block
      val normalizedHammingDistances : Seq[Double] = blocks.map{ block => normalizedHammingDistance(block.slice(0, keySize), block.slice(keySize, keySize*2)) }

      // Normalize for all blocks
      normalizedHammingDistances.sum/blocks.length.toDouble
    }
    hammingDistances.indexOf(hammingDistances.min) + keyStart
  }

  def subCiphertexts(ciphertext: Array[Byte], keySize : Int) : Array[Array[Byte]] = {
    val end = ciphertext.length
    (0 until keySize).map { start =>
      (start until end by keySize).map { index => ciphertext(index) }.toArray
    }.toArray
  }
}
