package Set3

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Exercise21 {
}

class MT19937(val seed: Int) {
  // The MT19937 algorithm requires two pieces of data to maintain state
  // 1) The current index for an array
  // 2) An array of values that is modified after 624 iterations
  var index = 624
  var state = MT19937.initMT(seed)

  def nextInt(): Int = {
    if (index >= 624) {
      state = MT19937.twist(state)
      index = 0
    }

    var y = state(index)
    y ^= (y >>> 11)
    y ^= (y << 7) & 0x9D2C5680
    y ^= (y << 15) & 0xEFC60000
    y ^= (y >>> 18)

    index += 1

    y
  }
}

object MT19937 {
  def initMT(seed: Int): Seq[Int] = {
    val mt: ArrayBuffer[Int] = ArrayBuffer.fill(624)(0)
    mt(0) = seed
    (1 until 624).foreach { i =>
      mt(i) = 1812433253 * (mt(i-1) ^ (mt(i-1) >>> 30)) + i
    }
    mt.toSeq
  }

  def twist(mt: Seq[Int]): Seq[Int] = {
    val newmt: mutable.Buffer[Int] =  mt.toBuffer
    (0 until 624).foreach { i =>
      val y: Int = (newmt(i) & 0x80000000) + (newmt((i+1) % 624) & 0x7fffffff)
      newmt(i) = newmt((i+397) % 624) ^ (y >>> 1)
      if ( y % 2 != 0 ) newmt(i) = newmt(i) ^ 0x9908b0df
    }
    newmt.toSeq
  }
}