package co.edu.icesi.u

import org.scalatest.funsuite.AnyFunSuite

class RadixSortTest extends AnyFunSuite {

  // Instancia de RadixSort para enteros
  val radixSort = new RadixSort[Int]

  test("sort should return an empty list for an empty input list") {
    assert(radixSort.sort(List()) == List())
  }

  test("sort should return the same list for a single element") {
    assert(radixSort.sort(List(5)) == List(5))
  }

  test("sort should return a sorted list for an already sorted list") {
    assert(radixSort.sort(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("sort should return a sorted list for a list in reverse order") {
    assert(radixSort.sort(List(5, 4, 3, 2, 1)) == List(1, 2, 3, 4, 5))
  }

  test("sort should return a sorted list for a random unsorted list") {
    assert(radixSort.sort(List(3, 1, 4, 1, 5, 9, 2, 6, 5)) == List(1, 1, 2, 3, 4, 5, 5, 6, 9))
  }
  test("sort should return a sorted list for a random unsorted list +") {
    assert(radixSort.sort(List(3, 1, 4, 1, 7, 8, 5, 9, 2, 6, 5)) == List(1, 1, 2, 3, 4, 5, 5, 6, 7, 8, 9))
  }
}
