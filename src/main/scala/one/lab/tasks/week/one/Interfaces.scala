package one.lab.tasks.week.one

/**
  * 1. You have to define three [[one.lab.tasks.week.one.Interfaces.Console]] class implementation, let's say Xbox,
  *    PlayStation, Sega
  * 2. You also need to define implementation GameDisk traits for each of console, and some classes of games, see
  *    [[one.lab.tasks.week.one.Interfaces.XboxGameDisk]] and [[one.lab.tasks.week.one.Interfaces.ForzaHorizon]]
  * 3. When creating implementation of Console be sure to properly implement play method,
  *    so that when I try to play Xbox with PS game disk, it will print me that disk format is invalid.
  *    But when I supply appropriate disk it will print s"playing ${disk.game()}"
  */
object Interfaces {

  trait Console {
    def play(disk: GameDisk): Unit
  }

  class Xbox extends Console{
    override def play(disk: GameDisk): Unit ={
      if (disk.consoleType == "Xbox") println(s"playing ${disk.game}")
      else println("The format of disk is invalid")
    }
  }

  class PlayStation extends Console{
    override def play(disk: GameDisk): Unit ={
      if (disk.consoleType == "PlayStation") println(s"playing ${disk.game}")
      else println("The format of disk is invalid")
    }
  }

  class Sega extends Console{
    override def play(disk: GameDisk): Unit ={
      if (disk.consoleType == "Sega") println(s"playing ${disk.game}")
      else println("The format of disk is invalid")
    }
  }

  trait GameDisk {
    val consoleType: String
    val game: String
  }

  trait XboxGameDisk extends GameDisk {
    override val consoleType: String = "Xbox"
  }

  trait PlayStationGameDisk extends GameDisk {
    override val consoleType: String = "PlayStation"
  }

  trait SegaGameDisk extends GameDisk{
    override val consoleType: String = "Sega"
  }

  class ForzaHorizon extends XboxGameDisk {
    override val game: String = "ForzaHorizon race game"
  }

  class CounterStrike extends PlayStationGameDisk{
    override val game: String = "Counter Strike Gloval Offensive"
  }

  class Dota extends SegaGameDisk {
    override val game: String = "Dota"
  }

  def main(args: Array[String]): Unit = {
    new Xbox().play(new ForzaHorizon())
    new Xbox().play(new CounterStrike())
    new Xbox().play(new Dota())
    new PlayStation().play(new ForzaHorizon())
    new PlayStation().play(new CounterStrike())
    new PlayStation().play(new Dota())
    new Sega().play(new ForzaHorizon())
    new Sega().play(new CounterStrike())
    new Sega().play(new Dota())
  }
}
