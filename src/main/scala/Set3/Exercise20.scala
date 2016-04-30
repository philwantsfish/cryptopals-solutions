package Set3

import java.util.Base64

import Set1.Exercise3
import Set3.Exercise18._
import Util.Util

import scala.io.Source


object Exercise20 {

  def decryptSetOfAESCTRWithSameNonce(datas: Seq[Array[Byte]]) = {
    // Make all the ciphertexts the size of the smallest one
    val shortest = datas.foldLeft(Integer.MAX_VALUE)((a,b) => if(b.length < a) b.length else a )
    val normalizedData = datas.map { d => d.slice(0, shortest) }

    // Transpose the ciphertexts, so each cipher text using a one byte key
    val transposedData = normalizedData.head.indices.map { index =>
      normalizedData.map { d => d(index) }
    }

    // For each ciphertext:
    //  * decrypt using all 255 possible keys
    //  * rank the possible plaintexts using frequency analysis
    //  * return the highest ranked one
    val almostthere = transposedData.map { d => Exercise3.decryptSingleByteXor(Util.toHexString(d)) }

    // Transpose the text back to their original positions
    val answers = almostthere.head.indices.map { index =>
      almostthere.map { d => d(index) }
    }

    answers
  }
}
