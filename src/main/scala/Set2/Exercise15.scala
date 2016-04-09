package Set2


object Exercise15 {
  def isValidPadding(input: Seq[Byte], blocksize: Int): Boolean = {
    val padding: Seq[Byte] = Seq.fill[Byte](input.last)(input.last)
    input.last > 0 && input.endsWith(padding) && (input.length % blocksize == 0)
  }
}
