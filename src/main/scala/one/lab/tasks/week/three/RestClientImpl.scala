package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.Materializer
import akka.util.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._

object RestClientImpl {

  def parseResponse(response: HttpResponse)(implicit materializer: Materializer, ex: ExecutionContext): Future[String] =
    response.entity.toStrict(10.seconds).flatMap { entity =>
      entity.dataBytes.runFold(ByteString.empty)((acc, bytes) => acc ++ bytes).map(_.utf8String)
    }

  def get(
      url: String,
      query: Option[Map[String, String]] = None,
      headers: Option[Map[String, String]] = None
  )(implicit system: ActorSystem, materializer: Materializer, ex: ExecutionContext): Future[String] = {
    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = url
    )
    Http().singleRequest(request).flatMap(parseResponse)
  }

  def post[T](
      url: String,
      data: T,
      query: Option[Map[String, String]] = None,
      headers: Option[Map[String, String]] = None
  )(implicit system: ActorSystem, materializer: Materializer, ex: ExecutionContext): Future[String] = {
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = url,
      entity = HttpEntity(contentType = ContentTypes.`application/json`, data.toString)
    )
    Http().singleRequest(request).flatMap(parseResponse)
  }

}
