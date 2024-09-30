package co.edu.icesi.u

import org.scalatest.funsuite.AnyFunSuite

class HeapSortTest extends AnyFunSuite {

  val heap = new HeapSort()

  //HeapSort -> del cap 6 text de Cormen
  test("Normal case") {
    val a = List(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val b = List(1, 2, 3, 4, 7, 8, 9, 10, 14, 16)
    val result = heap.sort(a)
    assert(result === b)

  }

  // Test case: Arrangement already ordered
  test("Arrangement already ordered") {
    val a = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result = heap.sort(a)
    assert(result === a)
  }

  //  Test case: Single-element array
  test("Single-element array") {
    val a = List(5)
    val result = heap.sort(a)
    assert(result === a)
  }

  //Test case: Empty arrangement
  test("Empty arrangement") {
    val a = List[Int]()
    val result = heap.sort(a)
    assert(result.isEmpty)
  }

  // Test case: Arrangement with all elements the same
  test("Arrangement with all elements the same") {
    val a = List(5, 5, 5, 5, 5, 5, 5)
    val result = heap.sort(a)
    assert(result === a)
  }
  
}
