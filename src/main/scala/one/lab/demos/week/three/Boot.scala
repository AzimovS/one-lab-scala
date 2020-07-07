package one.lab.demos.week.three

import java.io.FileNotFoundException
import java.rmi.ConnectIOException
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._

object Boot extends App {
  implicit val ex: ExecutionContext = ExecutionContext.global
  // failure
  // error

  def openFile(path: String): List[String] =
    throw new FileNotFoundException("sorry file not found")

//  var result: List[String] = null
//  try {
//    val lines = openFile("ditch")
//    result = lines
//  } catch {
//    case fail: FileNotFoundException =>
//    case err: NotImplementedError =>
//    case fail: Exception =>
//  }

  trait FileReadingError
  case class Interrupted() extends FileReadingError
  case class FileDeleted() extends FileReadingError

  val resultFile: Try[List[String]] = Try(openFile("ditch"))

  val result: String = resultFile match {
    case Success(value) => value.mkString(", ")
    case Failure(fail)  => fail.getMessage
  }

  val eitherResult: Either[FileReadingError, List[String]] =
    resultFile.toEither.left.map(x => Interrupted())

  resultFile.toOption
//  M                     F
//  |                     | init
//  | println(future)     | println("hello")

//  val future: Future[Int] = Future {
//    4
//  }

//  println(Await.result(future, 4.seconds))

  def getUser(id: Int): Future[String] =
    Future {
      Thread.sleep(3000)
      """{"user":"lalka"}"""
    }

  def getAccount(user: String): Future[String] =
    Future {
      throw new ConnectIOException("sorry db is dead")
    }

  def makeTransaction(accountId: String): Future[Int] =
    Future {
      Thread.sleep(2000)
      200
    }

  // callback hell
  // javascript                  N(js):1(OS)
  // goroutine                   M(go):N(OS)
  // koroutine JVM Memory model, 1(Java):1(OS)
  //
  //
  // Future[] -> [User getAccount() -> Future[String] ] -> Future[String]
  // 33 -> Future[String] => userJson -> Future[Account] => accountId -> makeTransaction(accountId) => Future[Int]
  // ACID -> Atomicity,
  // write - write -> (user_id, account_id, operation_id)
  // 12312321312312 -> 999999999

  getUser(33).onComplete {
    case Success(userId) =>
      getAccount(userId).onComplete {
        case Success(accountId) =>
          makeTransaction(accountId).onComplete {
            case Success(code) => println(code)
            case Failure(fail) => println(fail.getMessage)
          }
        case Failure(fail) => println(fail.getMessage)
      }
    case Failure(fail) => println(fail.getMessage)
  }

  getUser(33)
    .flatMap(userJson => getAccount(userJson))
    .recover { x =>
      println(x.getMessage)
      "default value"
    }
    .flatMap(accountId => makeTransaction(accountId))
    .onComplete {
      case Success(value) => println(value)
      case Failure(fail)  => println(fail.getMessage)
    }

  Thread.sleep(10000)

  // SBT - scala/simple build tool
  // Maven
}
