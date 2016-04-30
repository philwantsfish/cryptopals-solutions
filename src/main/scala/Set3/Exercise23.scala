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

  def cloneMT19937(mt: MT19937): MT19937 = {
    val seed = 1
    val mt = new MT19937(seed)

    val state: Seq[Int] = (0 until 624).map { i => untemper(mt.nextInt()) }

    val newmt = new MT19937(1)
    newmt.state = state

    newmt
  }
}
