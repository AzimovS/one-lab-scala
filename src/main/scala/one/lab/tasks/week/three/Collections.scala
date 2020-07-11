package one.lab.tasks.week.three

object Collections {

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    def duplicate(n: Int, newlist: List[A], list: List[A]): List[A] = {
      def ntimes(times: Int, num: A, list: List[A]): List[A] = {
        if (times == 0) list
        else ntimes(times - 1, num, list :+ num)
      }
      list match {
        case Nil => newlist
        case x :: tail => duplicate(n, ntimes(n, x, newlist), tail)
      }
    }
    duplicate(n, List(), list)
  }

  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))
  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    def split(k: Int, llist: List[A], rlist: List[A], list: List[A]): (List[A], List[A]) = {
      if (k == 0) {
        list match {
          case Nil => (llist, rlist)
          case x :: tail => split(k, llist, rlist :+ x, tail)
        }
      }
      else{
        list match {
          case Nil => (llist, rlist)
          case x :: tail => split(k - 1, llist :+ x, rlist, tail)
        }
      }
    }
    split(k, List(), List(), list)
  }

  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 2)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) = {
    if (k < 0) throw new java.lang.IndexOutOfBoundsException()
    def removeKth(k: Int, list: List[A]): A = {
      list match {
        case Nil => throw new java.lang.IndexOutOfBoundsException()
        case x :: tail => if (k == 0) x else removeKth(k - 1, tail)
      }
    }
    (list, removeKth(k, list))
  }


  def main(args: Array[String]): Unit = {
    println(duplicateNTimes(3, List(1,2,3)))
    println(duplicateNTimes(3, List()))
    println(splitAtK(4, List(1,2,3,4,5,6,7,8,9)))
    println(splitAtK(0, List(1,2,3)))
    println(removeKthElement(5, List(1,2,3,4,5,6)))
    println(removeKthElement(2, List(1,2,3,4,5,6)))
//    println(removeKthElement(-3, List(1,2,3,4,5,6)))
//    println(removeKthElement(1000, List(1,2,3,4,5,6)))
  }
}
