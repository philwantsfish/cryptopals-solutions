package Set3

import org.scalatest.{FlatSpec, Matchers}
import Exercise23._

class Exercise23Tests extends FlatSpec with Matchers {

  "Exercise23" should "implement unshiftRightXor" in {
    val z = 200
    var y = 200
    y ^= (y >>> 18)
    unshiftRightXor(y, 18).toHexString shouldBe z.toHexString
  }

  it should "implement unshiftRightXor2" in {
    val start = 0x87654821
    val end = start ^ (start >>> 11)
    unshiftRightXor(end, 11).toHexString shouldBe start.toHexString
  }

  it should "implement unshiftLeftAndSpecialXor" in {
    // Lets perform the operation on y and find what the real result is
    var y = 0xB75E7E72
    y ^= (y << 7) & 0x09D2C568
    y shouldBe 0xBE4C7F72

    // Check if the operation can take the result and get back y
    unshiftLeftAndSpecialXor(0xBE4C7F72, 7, 0x09D2C568) shouldBe 0xB75E7E72
  }

  it should "implement unshiftLeftAndSpecialXor2" in {
    var y = 0x9cb48a9a
    val transformed = y ^ (y << 7) & 0x9D2C5680

    unshiftLeftAndSpecialXor(transformed, 7, 0x9D2C5680) shouldBe 0x9cb48a9a
  }

  it should "be able to reverse each of the tempering steps" in {
    val stage0 = 0x12345678
    val stage1 = stage0 ^ (stage0 >>> 11)
    val stage2 = stage1 ^ (stage1 << 7) & 0x9D2C5680
    val stage3 = stage2 ^ (stage2 << 15) & 0xEFC60000
    val stage4 = stage3 ^ (stage3 >>> 18)
    var end4 = stage4

    val end3 = unshiftRightXor(end4, 18)
    end3 shouldBe stage3
    val end2 = unshiftLeftAndSpecialXor(end3, 15, 0xEFC60000)
    end2 shouldBe stage2
    val end1 = unshiftLeftAndSpecialXor(end2, 7, 0x9D2C5680)
    end1 shouldBe stage1
    val end = unshiftRightXor(end1, 11)
    stage0 shouldBe end
  }

  it should "implement untemper" in {
    val mt = new MT19937(1)

    val out0 = mt.nextInt()
    val state0 = mt.state.head
    untemper(out0) shouldBe state0

    val out1 = mt.nextInt()
    val state1 = mt.state(1)
    untemper(out1) shouldBe state1

    val out2 = mt.nextInt()
    val state2 = mt.state(2)
    untemper(out2) shouldBe state2
  }

  it should "be able to clone a mersenne twister starting from the index 0" in {
    val mt = new MT19937(1)
    val clonedmt = cloneMT19937FromIndex0(mt)

    // `mt` will twist and set index to 0 before generating the next number.
    // The cloned MT19937 already contains the twisted state and is index 0
    MT19937.twist(mt.state) shouldBe clonedmt.state

    // Confirm they generate the same numbers
    (1 to 700).foreach { i => mt.nextInt() shouldBe clonedmt.nextInt() }
  }

  it should "be able to clone a mersenne twister starting from the index 0 using ints" in {
    val mt = new MT19937(1)
    val ints = (1 to 624).map { i => mt.nextInt() }
    val clonedmt = cloneMT19937FromIndex0FromInts(ints)

    // Cloning this way required generating 624 ints. Lets fastforward the clonedmt to the same state
    // Confirm we generate the same numbers mt already generated
    (0 until 624).foreach { i => ints(i) shouldBe clonedmt.nextInt() }

    // Confirm they generate the same numbers onwards
    (1 to 700).foreach { i => mt.nextInt() shouldBe clonedmt.nextInt() }
  }

  it should "be able to clone a mersenne twister from a non zero index" in {
    val mt = new MT19937(1)
    (1 to 55).foreach { i => mt.nextInt() }
    mt.nextInt()
    mt.nextInt()

    val clonedMtOption = cloneMT19937(mt)

    val anothermt = new MT19937(1)
    (1 to 57).foreach {  i => anothermt.nextInt() }

    (1 to 1000).foreach { i => clonedMtOption.nextInt() shouldBe anothermt.nextInt() }
  }

  it should "clone an MT19937 from any index" in {
    (1 to 624).foreach { rand =>
      val mt = new MT19937(0x12345678)
      val mt2 = new MT19937(0x12345678)

      (1 to rand).foreach { i => mt.nextInt() }
      (1 to rand).foreach { i => mt2.nextInt() }

      val clonedmt = cloneMT19937(mt)

      (1 to 2000).foreach { i => clonedmt.nextInt() shouldBe mt2.nextInt() }
    }
  }
}