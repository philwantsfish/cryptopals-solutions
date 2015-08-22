package Set2

import scala.util.Random

object Exercise11 {
  def generateRandomKey(size: Int) : Seq[Byte] = { Seq.fill[Byte](size)(Random.nextInt(255).toByte) }

  def encryptWithRandomKey(data: Seq[Byte]) : Seq[Byte] = {
    //TODO: put 5-10 (randomize this too) random chars before and after the data.
    Seq()
  }

}
