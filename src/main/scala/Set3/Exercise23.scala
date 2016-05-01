package Set3


object Exercise23 {
  // The MT19937 nexInt applies a bitshift and xor operations on a state value
  // This "untemper" method reverses the operations yielding the original state value
  def untemper(output: Int): Int = {

    // the following is the logic we need to reverse
    //    y ^= (y >>> 11)
    //    y ^= (y << 7) & 0x9D2C5680
    //    y ^= (y << 15) & 0xEFC60000
    //    y ^= (y >>> 18)
    var y = output
    y = unshiftRightXor(y, 18)
    y = unshiftLeftAndSpecialXor(y, 15, 0xEFC60000)
    y = unshiftLeftAndSpecialXor(y, 7, 0x9D2C5680)
    y = unshiftRightXor(y, 11)
    y
  }

  /**
   * This function reverse the logic a right bitshfit Xor. For example:
   *  y = y ^ (y >>> 18)
   * Performing a right bitshift loses information and prepends with 0s. Because we are XORing with
   * the same value we shifted the lost information is preserved. The first 18 bits of y are the 18
   * bits lost during the shift.
   *
   *
   */
  def unshiftRightXor(y: Int, shiftAmount: Int): Int = {
    var original = 0x00000000
    val bitMask = 0xFFFFFFFF << (32 - shiftAmount)

    // Set the original shiftAmount of bits
    original = original | (bitMask & y)

    val iterations = if(32 % shiftAmount == 0) (32/shiftAmount) - 1 else 32/shiftAmount

    (1 to iterations).foreach { i =>
      val shiftedMask = bitMask >>> (i * shiftAmount)
      val originalMasked = (original >>> shiftAmount) & shiftedMask
      val resultMasked = y & shiftedMask
      original = original | (originalMasked ^ resultMasked)
    }

    original
  }


  /**
   * This one is much more tricky than unshiftRightXor because of the AND operation.
   *
   * The AND operation is not reversible. The bitshifts happens before the AND, causing zeros at the end.
   * ANDing the zeros results in zeros, then XORing with the original data results in the original data.
   *
   * The last shiftAmount of bits are the same as the original. Knowing this we can recover shiftAmount of
   * bits at a time. First, shift the shiftAmount of bits, AND them with the special number, then XOR with
   * the known result. This gives back another set of shiftAmount of original bits. Repeat this operation
   * until the full number are retrieved.
   */
  def unshiftLeftAndSpecialXor(y: Int, shiftAmount: Int, special: Int): Int = {
    var original = 0x00000000
    val bitMask = 0xFFFFFFFF >>> (32 - shiftAmount)

    // Set the original shiftAmount of bits
    original = original | (bitMask & y)

    val iterations = if(32 % shiftAmount == 0) (32/shiftAmount) - 1 else 32/shiftAmount

    (1 to iterations).foreach { i =>
      val shiftedMask = bitMask << (i * shiftAmount)
      val originalMasked = (original << shiftAmount) & shiftedMask
      val specialMasked = special & shiftedMask
      val resultMasked = y & shiftedMask
      original = original | ((originalMasked & specialMasked) ^ resultMasked)
    }

    original
  }

  /**
   * To clone a MT19337 we need to collect 624 consecutive integers starting from a fresh state.
   * Then, untempering the collected integers gives us a replica of the MT19937s state. Our clone
   * will start producing the same integers. Set the index to 0 because this state has already
   * been twisted. It will produce the same numbers as the passed in generator
   */
  def cloneMT19937FromIndex0(mt: MT19937): MT19937 = {
    val mt = new MT19937(1)

    val ints = (0 until 624).map { i => mt.nextInt() }
    val state = ints.map { i => untemper(i) }

    val newmt = new MT19937(1)
    newmt.state = state
    newmt.index = 0

    newmt
  }

  /** Same as the cloneMT19937FromIndex0 */
  def cloneMT19937FromIndex0FromInts(ints: Seq[Int]): MT19937 = {
    require(ints.size == 624)
    val state = ints.map { i => untemper(i) }

    val newmt = new MT19937(1)
    newmt.state = state
    newmt.index = 0

    newmt
  }

  def cloneMT19937(mt: MT19937): MT19937 = {
    val ints: Seq[Int] = (1 to 624).map { i => mt.nextInt() }
    val state: Seq[Int] = ints.slice(0, 624)
    cloneMT19937FromIndex0FromInts(state)
  }
}
