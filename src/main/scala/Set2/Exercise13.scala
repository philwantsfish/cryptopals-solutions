package Set2

import scala.util.matching.Regex


object Exercise13 {
  def sanitize(input: String) : String = { input.replaceAll("[&=]","") }

  def parsingRoutine(input: String) : String = {
    val intPattern = "(^\\d+$)".r
    val keyValueMap = input.split("&").map(kv => kv.split('='))
    val keyValueStrings = keyValueMap.map{ case Array(k,v) =>
      if(v.matches("^\\d+$")) "  " + k + ": " + v
      else "  " + k + ": '" + v + "'"
    }

    "{\n" ++ keyValueStrings.mkString(",\n") ++ "\n}"
  }

  def encodeProfile(profileMap: Map[String, String]) : String = {
    val encodedProfile = new StringBuilder()
    encodedProfile ++= "email=" + sanitize(profileMap("email"))
    encodedProfile ++= "&"
    encodedProfile ++= "uid=" + Integer.parseInt(profileMap("uid"))
    encodedProfile ++= "&"
    encodedProfile ++= "role=" + profileMap("role")
    encodedProfile.toString()
  }

  def profile_for(email: String) : String = {
    val profileMap: Map[String, String] = Map("email" -> sanitize(email), "uid" -> "10", "role" -> "user")
    encodeProfile(profileMap)
  }



}
