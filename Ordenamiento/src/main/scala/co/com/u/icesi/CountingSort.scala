package co.edu.icesi.u

import scala.annotation.tailrec

/**
 * Implements the Counting Sort algorithm for a generic type T.
 *
 * @tparam T the type of elements to be sorted, which must be ordered.
 * @param ord the implicit Ordering for the type T.
 */
class CountingSort[T](implicit ord: Ordering[T]) {
  import ord._

  /**
   * Sorts a list of elements using the Counting Sort algorithm.
   *
   * @param arr the list of elements to be sorted.
   * @param numeric the implicit Numeric instance for type T.
   * @return a sorted list of elements.
   * @throws NoSuchElementException if the list is empty.
   */
  def sort(arr: List[T])(implicit numeric: Numeric[T]): List[T] = {
    if (arr.isEmpty) arr
    else {
      val (minValue, maxValue) = findMinMax(arr)
      val range = numeric.toInt(numeric.minus(maxValue, minValue)) + 1
      val countList = fillCountList(arr, createList(range, 0), minValue)
      createSortedList(countList, minValue, List())
    }
  }

  /**
   * Finds the minimum and maximum values in a list.
   *
   * @param arr the list to search.
   * @return a tuple containing the minimum and maximum values.
   * @throws NoSuchElementException if the list is empty.
   */
  private def findMinMax(arr: List[T]): (T, T) = arr match {
    case Nil => throw new NoSuchElementException("List is empty")
    case head :: tail => findMinMaxRec(tail, head, head)
  }

  @tailrec
  private def findMinMaxRec(arr: List[T], min: T, max: T): (T, T) = arr match {
    case Nil => (min, max)
    case head :: tail =>
      val newMin = if (min < head) min else head
      val newMax = if (max > head) max else head
      findMinMaxRec(tail, newMin, newMax)
  }

  /**
   * Creates a list of a given size filled with a specified value.
   *
   * @param size the size of the list.
   * @param value the value to fill the list with.
   * @return a list of the specified size filled with the given value.
   */
  private def createList(size: Int, value: Int): List[Int] = {
    @tailrec
    def createListRec(count: Int, acc: List[Int]): List[Int] = {
      if (count <= 0) acc
      else createListRec(count - 1, value :: acc)
    }
    createListRec(size, Nil)
  }

  /**
   * Fills the count list based on the occurrences of elements in the input list.
   *
   * @param arr the input list.
   * @param countList the count list to be filled.
   * @param minValue the minimum value in the input list.
   * @param numeric the implicit Numeric instance for type T.
   * @return the filled count list.
   */
  @tailrec
  private def fillCountList(arr: List[T], countList: List[Int], minValue: T)(implicit numeric: Numeric[T]): List[Int] = arr match {
    case Nil => countList
    case head :: tail =>
      val index = numeric.toInt(numeric.minus(head, minValue))
      val updatedCountList = updateCountList(countList, index)
      fillCountList(tail, updatedCountList, minValue)
  }

  /**
   * Updates the count list at the specified index.
   *
   * @param countList the current count list.
   * @param index the index to be updated.
   * @return the updated count list.
   */
  private def updateCountList(countList: List[Int], index: Int): List[Int] = {
    @tailrec
    def updateRec(remaining: List[Int], currentIndex: Int, acc: List[Int]): List[Int] = remaining match {
      case Nil => reverse(acc)
      case head :: tail =>
        if (currentIndex == index)
          updateRec(tail, currentIndex + 1, (head + 1) :: acc)
        else
          updateRec(tail, currentIndex + 1, head :: acc)
    }
    updateRec(countList, 0, Nil)
  }

  /**
   * Creates a sorted list based on the count list.
   *
   * @param countList the count list.
   * @param minValue the minimum value for the current index.
   * @param result the accumulated result list.
   * @param numeric the implicit Numeric instance for type T.
   * @return the sorted list.
   */
  @tailrec
  private def createSortedList(countList: List[Int], minValue: T, result: List[T])(implicit numeric: Numeric[T]): List[T] = countList match {
    case Nil => result
    case head :: tail =>
      if (head > 0) {
        val updatedResult = addMultiple(result, minValue, head)
        createSortedList(tail, numeric.plus(minValue, numeric.fromInt(1)), updatedResult)
      } else {
        createSortedList(tail, numeric.plus(minValue, numeric.fromInt(1)), result)
      }
  }

  /**
   * Adds a value multiple times to the result list.
   *
   * @param result the current result list.
   * @param value the value to be added.
   * @param times the number of times to add the value.
   * @return the updated result list.
   */
  @tailrec
  private def addMultiple(result: List[T], value: T, times: Int): List[T] = {
    if (times <= 0) result
    else addMultiple(result :+ value, value, times - 1)
  }

  /**
   * Reverses the given list.
   *
   * @param list the list to be reversed.
   * @tparam T the type of elements in the list.
   * @return the reversed list.
   */
  private def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def reverseHelper(remaining: List[T], reversed: List[T]): List[T] = {
      remaining match {
        case Nil => reversed
        case head :: tail => reverseHelper(tail, head :: reversed)
      }
    }

    reverseHelper(list, Nil)
  }
}
