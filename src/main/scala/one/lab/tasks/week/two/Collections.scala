package one.lab.tasks.week.two

object Collections {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = ???

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = ???

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = ???

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): A = ???

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](list: List[A]): A = ???

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = ???

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): A = ???

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = ???

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = ???
}
