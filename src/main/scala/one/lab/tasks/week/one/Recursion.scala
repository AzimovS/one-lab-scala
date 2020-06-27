package one.lab.tasks.week.one

import scala.annotation.tailrec

object Recursion {
  def printNTimes(n: Int, value: String): Unit = {
    if (n == 1) println(value)
    else {
      println(value)
      printNTimes(n - 1, value)
    }
  }

  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def nthFibonacciNumber(n: Int): Int = {
    if (n < 1) 0
    else if (n < 2) 1
    else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)
  }

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    @tailrec
    def innertailrec(prevvalue: Int, curvalue: Int, cur: Int, until: Int): Int = {
      if (until < 1) prevvalue
      else if (cur >= until) curvalue
      else innertailrec(curvalue, prevvalue + curvalue, cur + 1, until)
    }
    innertailrec(0, 1, 1, n)
  }
}
