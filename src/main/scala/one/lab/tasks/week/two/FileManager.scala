package one.lab.tasks.week.two

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters._

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(error: String) extends Command
  case class ListDirectoryCommand()           extends Command
  case class ListFilesCommand()               extends Command
  case class ListAllContentCommand()          extends Command
  case class UnrecognizedCommand()            extends Command

  case class ChangeDirectoryCommand(destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)

  def getFiles(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isFile)
      .map(path => path.toFile.getName)
      .map(x => s"$path/$x")
      .toList
  }

  def getDirectories(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isDirectory)
      .map(path => path.toFile.getName)
      .map(x => s"$path/$x")
      .toList
  }
  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    if (path == ".."){
      val newpath = current.substring(0, current.lastIndexOf("/"))
      Right(newpath)
    }
    else {
      val newpath = current + "/" + path
      if (Files.isDirectory(Paths.get(newpath))) {
        Right(newpath)
      }
      else{
        Left(ChangePathError(s"$path No such file or directory"))
      }
    }
  }
  def parseCommand(input: String): Command = {
    if (input == "ll") ListAllContentCommand()
    else if (input == "ls") ListFilesCommand()
    else if (input == "dir") ListDirectoryCommand()
    else if (input.startsWith("cd")) ChangeDirectoryCommand(input.substring(3))
    else UnrecognizedCommand()
  }
  def handleCommand(command: Command, currentPath: String): String = {
    command match {
      case ListAllContentCommand() => getFiles(currentPath).mkString("\n") + getDirectories(currentPath).mkString("\n")
      case ListDirectoryCommand() => getDirectories(currentPath).mkString("\n")
      case ListFilesCommand() => getFiles(currentPath).mkString("\n")
      case ChangeDirectoryCommand(newpath) => changePath(currentPath, newpath) match {
        case Left(output) => output.error
        case Right(output) => output
      }
      case _ => "command not found"
    }
  }
  def main(basePath: String): Unit = {
    def recmain(path: String): Unit = {
      val command = parseCommand(scala.io.StdIn.readLine())
      val output = handleCommand(command, path)
      println(output)
      if (command.isSubstitutive) recmain(output)
      else recmain(path)
    }
    recmain(basePath)
  }
  main("/home/azimov/Desktop")
}
