package Set3

import org.scalatest.{Matchers, FlatSpec}
import Exercise22._

import scala.util.Random

class Exercise22Tests extends FlatSpec with Matchers {
  "Exercise22" should "Find the seed of MT19937 based on time" in {
    val seed = (System.currentTimeMillis() / 1000).toInt
    val randomInterval = Random.nextInt(10) * 1000
    Thread.sleep(randomInterval)
    val mt = new MT19937(seed)
    seed shouldBe findMT19937Seed(mt).get
  }
}
