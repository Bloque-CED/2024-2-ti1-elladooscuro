package co.com.u.icesi

import scala.annotation.tailrec

class RadixSort {

  def sort(arr: List[Int]): List[Int] = {
    if (arr.isEmpty) return arr

    val maxValue = findMax(arr)
    var exp = 1
    var sortedArr = arr

    while (maxValue / exp > 0) {
      sortedArr = countingSortByDigit(sortedArr, exp)
      exp *= 10
    }

    sortedArr
  }

  private def findMax(arr: List[Int]): Int = arr match {
    case Nil => 0
    case head :: tail => findMaxRec(tail, head)
  }

  @tailrec
  private def findMaxRec(arr: List[Int], max: Int): Int = arr match {
    case Nil => max
    case head :: tail =>
      val newMax = math.max(max, head)
      findMaxRec(tail, newMax)
  }

  private def countingSortByDigit(arr: List[Int], exp: Int): List[Int] = {
    val n = arr.length
    val output = new Array[Int](n)
    val count = new Array[Int](10)

    for (i <- 0 until 10) {
      count(i) = 0
    }

    for (i <- 0 until n) {
      val digit = (arr(i) / exp) % 10
      count(digit) += 1
    }

    for (i <- 1 until 10) {
      count(i) += count(i - 1)
    }

    for (i <- (n - 1) to 0 by -1) {
      val digit = (arr(i) / exp) % 10
      output(count(digit) - 1) = arr(i)
      count(digit) -= 1
    }

    var sortedList: List[Int] = List()
    for (i <- 0 until n) {
      sortedList = sortedList ::: List(output(i))
    }

    sortedList
  }

}