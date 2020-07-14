package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.util.Failure
import scala.util.Success

object Githuber extends App {
  implicit val system: ActorSystem                        = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer            = ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type        = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(login: String)
  case class GithubRepository(repoName: String)

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = {
    RestClientImpl.get(s"https://api.github.com/users/$username").map(body => parse(body).camelizeKeys.extract[GithubUser])
  }

  def getUserRepositories(repoUrl: String): Future[List[GithubRepository]] = {
    RestClientImpl.get(repoUrl).map(body => parse(body).camelizeKeys.extract[List[GithubRepository]])
  }

  def getUserInfo(username: String): Unit = {
    val repoUrl = s"https://api.github.com/users/$username/repos"
    getGithubUser(username).onComplete {
      case Success(result) => println(result, getUserRepositories(repoUrl))
      case Failure(err) => println("Error")
    }
  }

  getUserInfo("Azimovs")
}
