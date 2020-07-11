package one.lab.tasks.week.two


object Collections {
  //   getLast(List(1 ,2, 3, 4)) => 4
  //   getLast(List())           => java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = {
    list match {
      case Nil => throw new java.util.NoSuchElementException()
      case x :: Nil => x
      case x :: tail => getLast(tail)
    }
  }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case x :: Nil => Some(x)
      case x :: tail => getLastOption(tail)
    }
  }

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = {
    list match {
      case Nil => throw new java.util.NoSuchElementException()
      case x :: Nil => throw new java.util.NoSuchElementException()
      case x :: y :: Nil => x
      case x :: tail => getPreLast(tail)
    }
  }

  // getPreLastOption(List(1 ,2, 3, 4)) ->    Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case x :: Nil => None
      case x :: y :: Nil => Some(x)
      case x :: tail => getPreLastOption(tail)
    }
  }

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](findNum: Int, list: List[A]): A = {
    def nthElement(start: Int, find: Int, list: List[A]): A = {
      list match {
        case Nil => throw new java.lang.IndexOutOfBoundsException()
        case x :: tail => if (start == find) x else nthElement(start + 1, find, tail)
      }
    }

    nthElement(1, findNum, list)
  }

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = {
    def nthElement(start: Int, find: Int, list: List[A]): Option[A] = {
      list match {
        case Nil => None
        case x :: tail => if (start == find) Some(x) else nthElement(start + 1, find, tail)
      }
    }

    nthElement(1, n, list)
  }

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    def numElements(total: Int, list: List[A]): Int = {
      list match {
        case Nil => total
        case x :: tail => numElements(total + 1, tail)
      }
    }

    numElements(0, list)
  }

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    def reverseList(reversed: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => reversed
        case x :: tail => reverseList(x :: reversed, tail)
      }
    }
    reverseList(Nil, list)
  }

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    def duplicateList(duplicated: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => duplicated
        case x :: tail => duplicateList(duplicated :+ x :+ x, tail)
      }
    }
    duplicateList(List(), list)
  }

  def main(args: Array[String]): Unit = {
    println(getLast(List(1, 2, 3, 4)))
    //    println(getLast(List()))
    println(getLastOption(List(1, 2, 3, 4)))
    //    println(getLastOption(List()))
    println(getPreLast(List(1, 2, 3, 4)))
    //    println(getPreLast(List(1)))
    //    println(getPreLast(List()))
    println(getPreLastOption(List(1, 2, 3, 4)))
    //    println(getPreLastOption(List(1)))
    //    println(getPreLastOption(List()))
    println(getNthElement(3, List(1, 2, 3, 4)))
    //    println(getNthElement(3, List(1)))
    println(getNthElementOption(3, List(1, 2, 3, 4)))
    println(getNthElementOption(3, List(1)))
    println(getLength(List(1, 2, 3)))
    println(getLength(List()))
    println(getReversedList(List(1,2,3)))
    println(duplicateEveryElement(List(1,2,3)))
  }
}
