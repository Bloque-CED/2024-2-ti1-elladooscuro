package co.edu.icesi.u

import scala.annotation.tailrec

/**
 * Implements the Radix Sort algorithm for a generic type T.
 *
 * @tparam T the type of elements to be sorted, which must be orderable.
 * @param ord the implicit Ordering for the type T.
 */
class RadixSort[T](implicit ord: Ordering[T]) {
  import ord._

  /**
   * Sorts a list of elements using the Radix Sort algorithm.
   *
   * @param arr the list of elements to be sorted.
   * @param toInt a function that converts elements of type T to Int.
   * @return a sorted list of elements.
   */
  def sort(arr: List[T])(implicit toInt: T => Int): List[T] = {
    arr match {
      case Nil => arr // If the list is empty, return an empty list
      case _ =>
        val maxValue = findMax(arr)(toInt)
        sortRec(maxValue, 1, arr)(toInt)
    }
  }

  /**
   * Recursively sorts the list based on each digit, starting from the least significant digit.
   *
   * @param maxValue the maximum value in the list.
   * @param exp the current exponent to isolate the digit to sort by.
   * @param arr the list to be sorted.
   * @param toInt a function that converts elements of type T to Int.
   * @return a sorted list of elements.
   */
  @tailrec
  private def sortRec(maxValue: Int, exp: Int, arr: List[T])(implicit toInt: T => Int): List[T] = {
    (maxValue / exp) match {
      case 0 => arr // If maxValue / exp is 0, return arr
      case _ =>
        val sortedArr = countingSortByDigit(arr, exp)(toInt)
        sortRec(maxValue, exp * 10, sortedArr)(toInt)
    }
  }

  /**
   * Finds the maximum value in the list.
   *
   * @param arr the list to search.
   * @param toInt a function that converts elements of type T to Int.
   * @return the maximum value in the list.
   */
  private def findMax(arr: List[T])(implicit toInt: T => Int): Int = {
    arr match {
      case Nil => 0
      case head :: tail => findMaxRec(tail, toInt(head))
    }
  }

  /**
   * Recursively finds the maximum value in the list.
   *
   * @param arr the list to search.
   * @param max the current maximum value found.
   * @param toInt a function that converts elements of type T to Int.
   * @return the maximum value in the list.
   */
  @tailrec
  private def findMaxRec(arr: List[T], max: Int)(implicit toInt: T => Int): Int = {
    arr match {
      case Nil => max
      case head :: tail =>
        findMaxRec(tail, math.max(max, toInt(head)))
    }
  }

  /**
   * Sorts the list based on the digit at the specified exponent using Counting Sort.
   *
   * @param arr the list to be sorted.
   * @param exp the exponent to isolate the digit.
   * @param toInt a function that converts elements of type T to Int.
   * @return a sorted list of elements based on the current digit.
   */
  private def countingSortByDigit(arr: List[T], exp: Int)(implicit toInt: T => Int): List[T] = {
    val count = List.fill(10)(0)

    val updatedCount = countDigits(arr, exp, count)(toInt)

    val accumulatedCount = accumulateCount(updatedCount)

    buildOutput(arr, exp, accumulatedCount, List.fill(arr.length)(null.asInstanceOf[T]), arr.length - 1)(toInt)
  }

  /**
   * Counts the occurrences of each digit in the input list.
   *
   * @param arr the input list.
   * @param exp the exponent to isolate the digit.
   * @param count the current count of each digit.
   * @param toInt a function that converts elements of type T to Int.
   * @return the updated count of digits.
   */
  @tailrec
  private def countDigits(arr: List[T], exp: Int, count: List[Int])(implicit toInt: T => Int): List[Int] = {
    arr match {
      case Nil => count
      case head :: tail =>
        val digit = (toInt(head) / exp) % 10
        countDigits(tail, exp, updateCount(count, digit, count(digit) + 1))
    }
  }

  /**
   * Accumulates the count to provide the positions of the digits in the output list.
   *
   * @param count the count of each digit.
   * @return the accumulated count.
   */
  private def accumulateCount(count: List[Int]): List[Int] = {
    @tailrec
    def accumulateRec(i: Int, currentCount: List[Int], result: List[Int]): List[Int] = {
      if (i >= currentCount.length) result
      else {
        val updatedResult = updateCount(result, i, result(i) + result(i - 1))
        accumulateRec(i + 1, currentCount, updatedResult)
      }
    }

    accumulateRec(1, count, count)
  }

  /**
   * Builds the output list based on the accumulated count and the input list.
   *
   * @param arr the input list.
   * @param exp the exponent to isolate the digit.
   * @param count the accumulated count.
   * @param output the output list being built.
   * @param i the current index in the input list.
   * @param toInt a function that converts elements of type T to Int.
   * @return the sorted output list.
   */
  @tailrec
  private def buildOutput(arr: List[T], exp: Int, count: List[Int], output: List[T], i: Int)(implicit toInt: T => Int): List[T] = {
    if (i < 0) output
    else {
      val digit = (toInt(arr(i)) / exp) % 10
      val index = count(digit) - 1
      buildOutput(arr, exp, updateCount(count, digit, index), updateCount(output, index, arr(i)), i - 1)(toInt)
    }
  }

  /**
   * Updates the count list at the specified index with a new value.
   *
   * @param count the current count list.
   * @param index the index to be updated.
   * @param newValue the new value to place at the specified index.
   * @tparam T the type of elements in the count list.
   * @return the updated count list.
   */
  private def updateCount[T](count: List[T], index: Int, newValue: T): List[T] = {
    count match {
      case Nil => List(newValue) // If the list is empty, add the new value
      case head :: tail =>
        index match {
          case 0 => newValue :: tail // If we've reached the index, replace the value
          case _ => head :: updateCount(tail, index - 1, newValue) // Recursive call to continue
        }
    }
  }
}
