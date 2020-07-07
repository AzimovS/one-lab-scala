package one.lab.demos.week.two

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.stream

import scala.concurrent.Future
import scala.jdk.CollectionConverters._

object FileManagerDemo extends App {
  // Stream(1,2,2,3,4,5,56, ...............)
  val path = "/Users/artur"

  val list: Iterator[String] =
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isFile)
      .map(path => path.toFile.getName)
      .map(x => s"$path/$x")

//      .map(path => path.toFile.getName)

  list.foreach(println)

}
