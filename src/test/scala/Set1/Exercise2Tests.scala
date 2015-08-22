package Set1

import Exercise2._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class Exercise2Tests extends JUnitSuite {
  @Test
  def testMatasano2 = {
    val message1 = "1c0111001f010100061a024b53535009181c"
    val message2 = "686974207468652062756c6c277320657965"
    val answer   = "746865206b696420646f6e277420706c6179".toUpperCase
    assert(xor(message1, message2) == answer, s"${xor(message1, message2)} did not match ${answer}")
  }
}
