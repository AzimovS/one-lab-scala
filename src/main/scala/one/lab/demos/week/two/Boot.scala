package one.lab.demos.week.two

object Boot extends App {
  import Utils._

  // higher order functions
  // function - first class citizien
  def showToConsole(value: String, printToConsole: String => Unit): Unit =
    printToConsole(value)

  val printToStdOut: String => Unit = str => println(str)
  showToConsole("hello", printToStdOut)

  List(1, 2, 3, 4).map(x => x * 3)

  // implicit params
  // implicit - неявный
  def showToConsole2(value: String)(implicit printToConsole: String => Unit): Unit =
    printToConsole(value)

  showToConsole2("value")

  // implicit conversions

  def printToConsole3(value: String): Unit = println(value)

  printToConsole3(19)
  printToConsole3(3.0d)

  // implicit extensions
  // OOP pattern that wraps existing code and adds new method
  10.plus(4)

  // case class vs class
  val car               = Car.apply("some type", 2000)
  val car2              = Car("some type", 2000)
  val carWithCaseClass  = CarWithCaseClass("some type", 2000, "red")
  val carWithCaseClass2 = CarWithCaseClass("some type", 2000, "red")

  println(car.toString)
  println(carWithCaseClass.toString)
  println(car == car2)
  println(carWithCaseClass == carWithCaseClass)
  println("111" == "111") // false in java, true in scala

//  implicit val stringNumeric: Numeric[String] = new Numeric[String] {
//    override def plus(x: String, y: String): String = x + " " + y
//
//    override def minus(x: String, y: String): String = ???
//
//    override def times(x: String, y: String): String = ???
//
//    override def negate(x: String): String = ???
//
//    override def fromInt(x: Int): String = ???
//
//    override def toInt(x: String): Int = ???
//
//    override def toLong(x: String): Long = ???
//
//    override def toFloat(x: String): Float = ???
//
//    override def toDouble(x: String): Double = ???
//
//    override def compare(x: String, y: String): Int = ???
//
//    override def parseString(str: String): Option[String] = Some(str)
//  }

  // companion object
  // see [[one.lab.demos.week.two.Utils]]
//  List[String]("1")
//
//  List[Int](1, 2, 3).sum
//  println(List[String]("1", "2").sum)

  println(List(1, 2, 3, 4).map(x => s"Integer $x"))

  def half(value: Int): Option[Int] =
    if (value / 2 == 0) None
    else Some(value / 2)

//  Functor map ->
//   --> 30 --> Option(15)                    --> Option(Option(7))
//                      half(15) --> Option(7)
//  val someHalfValue: Option[Option[Int]] = half(30).map(x => half(x))

//  --> 30  --> Option(15)                  --> Option(7)
//                      half(15) -> Option(7)
//  val someHalfValue: Option[Int] = half(30).flatMap(x => half(x))

//  val value              = 2
//  val someHalfValue: Int = if (value / 2 == 0) null else value / 2

// imperative code - как надо делать
// declarative code - что нужно делать

  List(1, 2, 3, 4, 5).map(x => x * 3).filter(x => x % 2 != 0).map(x => s"got $x")
}
