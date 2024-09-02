package co.com.u.icesi

import org.scalatest.funsuite.AnyFunSuite

class HeapSortTest {

  val heap = new HeapSort()

  //HeapSort -> del cap 6 text de Cormen
  test("Normal case") {
    val a = Array(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val b = Array(1, 2, 3, 4, 7, 8, 9, 10, 14, 16)

    val result = heap.heapSort(a)
    assert(result === b)

  }

  // Test case: Arrangement already ordered
  test("Arrangement already ordered") {
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result = heap.heapSort(a)
    assert(result === a)
  }

  // Test Case: Arrangement in descending order
  test("Test Case: Arrangement in descending order") {
    val a = Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    val b = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result = heap.heapSort(a)
    assert(result === b)
  }

  // Test case: Arrangement with repeated elements
  test("Arrangement with repeated elements") {
    val a = Array(4, 10, 3, 10, 2, 10, 1, 4)
    val b = Array(1, 2, 3, 4, 4, 10, 10, 10)
    val result = heap.heapSort(a)
    assert(result === b)
  }

  //  Test case: Single-element array
  test("Single-element array") {
    val a = Array(5)
    val result = heap.heapSort(a)
    assert(result === a)
  }

  //Test case: Empty arrangement
  test("Empty arrangement") {
    val a = Array[Int]()
    val result = heap.heapSort(a)
    assert(result.isEmpty)
  }

  // Test case: Arrangement with all elements the same
  test("Arrangement with all elements the same") {
    val a = Array(5, 5, 5, 5, 5, 5, 5)
    val result = heap.heapSort(a)
    assert(result === a)
  }
  
}