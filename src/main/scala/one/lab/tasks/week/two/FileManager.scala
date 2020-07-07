package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths

import scala.jdk.CollectionConverters._
import scala.util.chaining._

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

  case class ChangeDirectoryCommand(destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)

  def getFiles(path: String): List[String]                                       = ???
  def getDirectories(path: String): List[String]                                 = ???
  def changePath(current: String, path: String): Either[ChangePathError, String] = ???
  def parseCommand(input: String): Command                                       = ???
  def handleCommand(command: Command, currentPath: String): String               = ???
  def main(basePath: String): Unit                                               = ???
}
