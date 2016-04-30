package Set3

import org.scalatest.{FlatSpec, Matchers}

class Exercise21Tests extends FlatSpec with Matchers {

  "Exercise21.scala" should "implment MT19937" in {
    // Test data taken from https://github.com/cslarsen/mersenne-twister
    // The above dataset is using unsigned integers, but the JVM doesn't have unsigned integers.
    // To use this test data we will cast the output to long removing the sign bit
    val mt = new MT19937(1)
    1791095845L shouldBe mt.nextInt() & 0xFFFFFFFFL
    4282876139L shouldBe mt.nextInt() & 0xFFFFFFFFL
    3093770124L shouldBe mt.nextInt() & 0xFFFFFFFFL
    4005303368L shouldBe mt.nextInt() & 0xFFFFFFFFL
    491263L shouldBe mt.nextInt() & 0xFFFFFFFFL
  }
}
