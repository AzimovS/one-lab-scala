package one.lab.demos.week.three

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization.write

object JsonPlayground extends App {
  implicit val formats: DefaultFormats.type = DefaultFormats

  case class Account(name: String, lastName: Option[String])
  // XML - < >
  // JSON - JavaScript Object Notation

  val jsonString: String = """{"name":"Azamat","last_name":"Rsymbetov"}"""

  val account: Account = parse(jsonString).camelizeKeys.extract[Account]
  println(account)

  val jsonRawString = write(account)
  println(jsonRawString)

}
