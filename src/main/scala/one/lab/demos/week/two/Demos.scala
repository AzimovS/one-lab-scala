package one.lab.demos.week.two

import java.util.NoSuchElementException

object Demos extends App {
  // trait List
  // case object Nil -> empty list
  // case class ::

  trait MyList[T] {
    def fmap[B](fun: T => B): MyList[B]
  }

  case class Sequen[T](head: T, tail: MyList[T]) extends MyList[T] {
    override def fmap[B](fun: T => B): MyList[B] = Sequen(fun(head), tail.fmap(fun))
  }

  case class Empty[T]() extends MyList[T] {
    override def fmap[B](x: T => B): MyList[B] = Empty()
  }

//  implicit class Mapper[T](list: MyList[T]) {
//
//    def fmap[B](f: T => B): MyList[B] =
//      list match {
//        case Sequen(head, tail) => Sequen(f(head), tail.fmap(f))
//        case Empty()           => Empty()
//      }
//
//  }

  println(Sequen(1, Sequen(2, Sequen(3, Empty()))).fmap(x => x * 3))
  println(List[Int](1, 2, 3).map(x => x * 3))

//  println(List() match {
//    case Nil          => "0 element"
//    case head :: tail => "1 element and tail"
//  })
//  [0 -> 1 -> 2 -> 3]
//       x  tail
//  List(1, List(2, List(3, Nil)))
//  ::.(1, List(3, Nil))
  // first task
  def getLast[A](list: List[A]): A =
    list match {
      case Nil                       => throw new NoSuchElementException()
      case x :: tail if tail.isEmpty => x
      case _ :: tail                 => getLast(tail)
    }

//  println(getLast(List.empty[String]))
  println(getLast(List("1")))
  println(getLast(List("1", "2", "3")))
  //     List(1, List(3, Nil)
  println(::(1, List(3)))
}
