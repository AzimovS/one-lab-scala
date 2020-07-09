package one.lab.demos.week.three

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethod
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.github.arturka.chat.library.LinkAttachment
import io.github.arturka.chat.library.Message
import org.json4s.native.JsonMethods.parse
import org.json4s.DefaultFormats
import requests.Response
import sttp.client._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object BootHttpClient extends App {
  implicit val system       = ActorSystem("lalka")
  implicit val materializer = ActorMaterializer.create(system)
  implicit val ex           = ExecutionContext.global
  //  Scala Collection API
  println("Sync")
  case class GithubUser(id: Long, login: String, avatarUrl: Option[String], reposUrl: String, email: Option[String])

  implicit val formats = DefaultFormats

  val response = parse(requests.get("https://api.github.com/users/arturka").text).camelizeKeys.extract[GithubUser]
  println(response)

  val response1 =
    parse(requests.get("https://api.github.com/users/nullpointer666").text).camelizeKeys.extract[GithubUser]

  println(response1)
//  val response2: String = requests.get("https://api.github.com/users/arturka").text
//  val response3: String = requests.get("https://api.github.com/users/arturka").text
//

  println("-" * 10)
  println("Async")

//  println(response3)
  def parseResponse(response: HttpResponse): Future[String] =
    response.entity.toStrict(3.seconds).flatMap { entity =>
      entity.dataBytes.runFold(ByteString.empty)((acc, bytes) => acc ++ bytes).map(_.utf8String)
    }

  val requestArturUser =
    Http()
      .singleRequest(HttpRequest(HttpMethods.GET, Uri("https://api.github.com/users/arturka")))
      .flatMap(response => parseResponse(response))
      .map(body => parse(body).camelizeKeys.extract[GithubUser])

  val requestNullPointerUser: Future[GithubUser] =
    Http()
      .singleRequest(HttpRequest(HttpMethods.GET, Uri("https://api.github.com/users/nullpointer666")))
      .flatMap(response => parseResponse(response))
      .map(body => parse(body).camelizeKeys.extract[GithubUser])

  requestArturUser.foreach(x => println(x))
  requestNullPointerUser.foreach(x => println(x))

}
