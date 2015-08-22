package Set2

object Exercise9 {

  def PKCS7(input: String, blocksize: Int) : Seq[Byte] = { PKCS7(input.getBytes, blocksize) }
  def PKCS7(input: Seq[Byte], blocksize: Int) : Seq[Byte] = {
    val padLength = blocksize - (input.length % blocksize)
    if(padLength != 0) input ++ Seq.fill[Byte](padLength)(padLength.toByte)
    else input ++ Seq.fill[Byte](blocksize)(blocksize.toByte)
  }
}
