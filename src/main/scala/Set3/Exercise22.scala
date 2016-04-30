package Set3

object Exercise22 {

  def findMT19937Seed(mt: MT19937): Option[Int] = {
    // We are assuming the passed in MT19937 PRNG seed was based on the current time when constructed
    // Let our initial guess be the current time and work backwards
    val currentTime = (System.currentTimeMillis / 1000).toInt

    // The integer to check our guess against
    val firstInt = mt.nextInt()

    // For each guess seed value, create a MT119337 and check if it produces the correct value
    // Lets also assume the seed was made with the last month
    val oldSeedValue = currentTime - (30 * 24 * 60 * 60)
    (currentTime to oldSeedValue by -1).find { seedGuess =>
      val mtGuess = new MT19937(seedGuess)
      mtGuess.nextInt() == firstInt
    }
  }
}
