package one.lab.demos.week.two

//import scala.concurrent.Future

class Whale(val color: String)

object Whale {
  def apply(color: String): Whale = new Whale(color)

  def unapply(whale: Whale): Option[String] = Some(whale.color)
}

object BootPractice extends App {

  // type parameters
  trait Figure {
    def getPrefix: String = "The shape is "
  }

  class Square[T](value: T) extends Figure { self =>
    def get(): T        = value
    def set(a: T): Unit = println(super.getPrefix)
    def getType: String = self.value.getClass.getName

    override def toString: String = super.toString
  }

  trait Animal {}

  trait A extends Animal
  trait B extends Animal

//  case class Some() extends A with B

  case class Dog(name: String, bark: String) extends Animal {
    def apply(name: String, bark: String): Dog = new Dog(name, bark)
  }

  case class Cat(name: String, meow: String) extends Animal

  val list: List[Animal] = List(Dog("", ""), Cat("", ""))
  case class Box[+T](value: T)

  val dogInBox           = Box(Dog("lalka", "whooh"))
  val catInBox: Box[Cat] = Box(Cat("", ""))

  //  Cat, Dog < Animal
  //  Box[Cat] < Box[Animal] variance problem - Covariant
  //  Box[Cat] != Box[Animal]                 - Invariant
  //  Box[Animal] < Box[Cat]                  - Contravariant
  def getInnerAnimal(animal: Box[Animal]): Animal =
    animal.value

  getInnerAnimal(dogInBox)

  // pattern matching
  // switch case
  // if else if else else
//  switch("asdfas"):
//    case "Meiram":
//      println("")
//    case "Baha":
//      println("")
//    case "Donald":
//      println("Trump")
//    default:
//      println("defualt")

  "Donald" match {
    case "Baha"    => "Zhiga"
    case "Donald"  => "Trump"
    case "dasadas" => "asdasd"
    case _         => "DEFAULT"
  }

  val dog = Dog("name", "whowh")

  def checkType(animal: Animal): Unit =
    animal match {
      case Whale(color)    => println(s"$color")
      case Dog(name, bark) => println(s"$name $bark")
      case c: Cat          => println(s"${c.name} ${c.meow}")
      case _               => println("unhandled")
    }

//  def checkTypeOldSchool(animal: Animal): Unit = {
//    if (animal.isInstanceOf[Dog]) {
//      val dog = animal.asInstanceOf[Dog]
//      dog.name
//    } else if
//  }

  checkType(dog)

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None
    else Some(a / b)

//  divide(4 / 2, 3 + 31)
//  Context[side effect] - printing, http requests, connect to database

//  1. same output for same input
//  2. no side effects
//  3. referential transparency

  // Monad - context defined in scala
  List(1, 2, 3, 5) // sequence
//  Option()            // emptiness of value
//  Either[String, Int] // wraps exception
// Future[String]    // wraps async task
// IO, State[], Reader[], Writer[]
  case class Account(id: Int)
  case class Balance(amount: Double)
  type Result = Int
  def makeTransaction(account: Account, balance: Balance): Either[String, Option[Result]] = ???

// List[String]

  val result: Either[String, Option[Int]] = makeTransaction(Account(2), Balance(3))
}
