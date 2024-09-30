package co.edu.icesi.u

import org.scalatest.funsuite.AnyFunSuite

class CountingSortTest extends AnyFunSuite {

  test("sort empty list") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List()) === List())
  }

  test("sort single element list") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(5)) === List(5))
  }

  test("sort already sorted list") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
  }

  test("sort unsorted list") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(4, 2, 7, 1, 9, 3)) === List(1, 2, 3, 4, 7, 9))
  }

  test("sort list with duplicates") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(4, 2, 4, 1, 9, 2)) === List(1, 2, 2, 4, 4, 9))
  }

  test("sort negative numbers") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(-5, -1, -3, 0, 2)) === List(-5, -3, -1, 0, 2))
  }

  test("sort mixed positive and negative numbers") {
    val countingSort = new CountingSort[Int]()
    assert(countingSort.sort(List(-10, 5, 0, -3, 2, -1)) === List(-10, -3, -1, 0, 2, 5))
  }
}
