package Set1

import Util.Util._

object Exercise8 {

  def getDuplicateBlockCount(ciphertext: String, keySize: Int) : Int = {
    val blocks: Array[String] = ciphertext.grouped(keySize*2).toArray
    val blockCountArray: Array[(String, Int)] = blocks.groupBy(b => b).map{ case(k,v) => (k, v.length) }.toArray
    blockCountArray.sortWith(_._2 > _._2).head._2
  }

  def getDuplicateBlockCount(ciphertext: Array[Byte], keySize: Int) : Int = { getDuplicateBlockCount(toHexString(ciphertext), keySize*2) }

  def detectAllECBData(ciphertexts: Array[String], keySize: Int) : Array[(Int, String)]= {
    ciphertexts.map{ ciphertext => (getDuplicateBlockCount(ciphertext, keySize), ciphertext) }.filter{ _._1 > 0}
  }
}
