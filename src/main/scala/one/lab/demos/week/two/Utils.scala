package one.lab.demos.week.two

object Utils {
  implicit val printToStdOut2: String => Unit = str => println(str)

  implicit class Plus(int: Int) {
    def plus(other: Int): Int = int + other
  }

  implicit def intToString(int: Int): String       = int.toString
  implicit def doubleToString(int: Double): String = int.toString
}
