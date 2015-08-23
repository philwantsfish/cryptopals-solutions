package Set2

import javax.crypto.Cipher

import Set1.Exercise6Tests
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import Exercise10._
import scala.io.Source
import Util.Util._

class Exercise10Tests extends JUnitSuite {
  @Test
  def testECBBlockEncrypt() = {
    val block  =  hexStringToByteArray("48656C6C6F2C20667269656E642E2048")
    val iv =  hexStringToByteArray("00000000000000000000000000000000")
    val key = "YELLOW SUBMARINE".getBytes

    val answer = encryptBlock("AES/ECB/NoPadding", key, xor(block, iv))
    assert(toHexString(answer.toArray) === "85BE107C273D715BD62C3E4CAF654AD3")
  }

  @Test
  def testECBBlockDecrypt() = {
    val block  =  hexStringToByteArray("85BE107C273D715BD62C3E4CAF654AD3")
    val key = "YELLOW SUBMARINE".getBytes
    val answer = decryptBlock("AES/ECB/NoPadding", key, block)
    assert(toHexString(answer.toArray) === "48656C6C6F2C20667269656E642E2048")
  }

  @Test
  def testECBEncrypt() = {
    val key = "YELLOW SUBMARINE".getBytes
    val data = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that.".getBytes
    val solution = "85be107c273d715bd62c3e4caf654ad35023268ef2e118fb3a97232284978f0b8bc59899c6c6bf15b21faea08ef8a92d8b228320b1355fbc4af6a5e451a2cb1c1307fb59928fd21f7748609feb899afd76b6b80621dc9d73dbecd4fb363665e39d89efd38422713c04c7c1eb6651a86f6023cedea27a1087bd5f6ee769d456338f239003cba54a87fe9722ebfa6a8cc2c2c89c501262ce23f27ab013d712377f"
    val attempt = encryptECB(data, key, "AES/ECB/NoPadding")
    assert(toHexString(attempt.toArray).toLowerCase == solution)
  }

  @Test
  def testECBDecrypt() = {
    val key = "YELLOW SUBMARINE".getBytes
    val data = hexStringToByteArray("85be107c273d715bd62c3e4caf654ad35023268ef2e118fb3a97232284978f0b8bc59899c6c6bf15b21faea08ef8a92d8b228320b1355fbc4af6a5e451a2cb1c1307fb59928fd21f7748609feb899afd76b6b80621dc9d73dbecd4fb363665e39d89efd38422713c04c7c1eb6651a86f6023cedea27a1087bd5f6ee769d456338f239003cba54a87fe9722ebfa6a8cc2c2c89c501262ce23f27ab013d712377f")
    val solution = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that."
    val attempt = decryptECB(data, key, "AES/ECB/NoPadding")
    assert(toASCIIString(attempt.toArray) == solution)
  }

  @Test
  def testCBCEncrypt() = {
    val data = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that.".getBytes
    val key : Seq[Byte] = "YELLOW SUBMARINE".getBytes
    val IV : Seq[Byte] = Seq.fill[Byte](key.length)(0.toByte)

    val solution = hexStringToByteArray("85be107c273d715bd62c3e4caf654ad378c2a34c1b76dc99157a46bd78bec878cb6895e68e58f36ee17561b96f5145ed2c9e20287218ab04fd80d3f9797997d7c691be2e42787605be4a4aed9c9a6f275b7abc5d6a2fb35681e0661b969c835c647c86ae669ede00556cdacfbc48cdd8a66da71342647b6480a4ec39d9e4ad8fd116e5666de49f76b081ae2e719835e1A951DE740E9C28FF95EB81D57E86C3AD")
    val answer: Seq[Byte] = encryptCBC(data, key, IV, "AES/ECB/NoPadding")
    assert(toHexString(solution.toArray) == toHexString(answer.toArray))
  }

  @Test
  def testCBCDecrypt() = {
    val solution = "Hello, friend. Hello, friend. That's lame. Maybe I should give you a name. But that's a slippery slope. You're only in my head. We have to remember that.".getBytes
    val key : Seq[Byte] = "YELLOW SUBMARINE".getBytes
    val IV : Seq[Byte] = Seq.fill[Byte](key.length)(0.toByte)
    val data = hexStringToByteArray("85be107c273d715bd62c3e4caf654ad378c2a34c1b76dc99157a46bd78bec878cb6895e68e58f36ee17561b96f5145ed2c9e20287218ab04fd80d3f9797997d7c691be2e42787605be4a4aed9c9a6f275b7abc5d6a2fb35681e0661b969c835c647c86ae669ede00556cdacfbc48cdd8a66da71342647b6480a4ec39d9e4ad8fd116e5666de49f76b081ae2e719835e1A951DE740E9C28FF95EB81D57E86C3AD")
    val answer: Seq[Byte] = decryptCBC(data, key, IV, "AES/ECB/NoPadding")
    assert(toHexString(solution.toArray) == toHexString(answer.toArray))
  }

  @Test
  def testMatasano() = {
    val base64data : String = Source.fromURL(getClass.getResource("/Exercise10TestData.txt")).getLines().mkString
    val data : Seq[Byte] = java.util.Base64.getDecoder.decode(base64data)
    val key : Seq[Byte] = "YELLOW SUBMARINE".getBytes
    val IV : Seq[Byte] = Seq.fill[Byte](key.length)(0.toByte)
    val answer: Seq[Byte] = decryptCBC(data, key, IV, "AES/ECB/NoPadding")
    assert(toASCIIString(answer.toArray) == Exercise6Tests.matasanoAnswer)
  }


}
