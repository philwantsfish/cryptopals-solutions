package Set1

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object Exercise7 {
  def decryptAESECB(data: Array[Byte], keyBytes: Array[Byte]) : Array[Byte] = {
    val cipher : Cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
    val key : SecretKeySpec = new SecretKeySpec(keyBytes, "AES")
    cipher.init(Cipher.DECRYPT_MODE, key)
    cipher.doFinal(data)
  }
}
