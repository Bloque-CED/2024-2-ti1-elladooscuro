package co.com.u.icesi

import org.scalatest.funsuite.AnyFunSuite

class HeapSortTest extends AnyFunSuite {

  val heap = new HeapSort()

  //HeapSort -> del cap 6 text de Cormen
  test("Normal case") {
    val a = List(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val b = List(1, 2, 3, 4, 7, 8, 9, 10, 14, 16)

    heap.add(a)
    val result = heap.sort()
    assert(result === b)

  }

  // Test case: Arrangement already ordered
  test("Arrangement already ordered") {
    val a = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    heap.add(a)
    val result = heap.sort()
    assert(result === a)
  }

  // Test Case: Arrangement in descending order
  test("Test Case: Arrangement in descending order") {
    val a = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    val b = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    heap.add(a)
    val result = heap.sort()
    assert(result === b)
  }

  // Test case: Arrangement with repeated elements
  test("Arrangement with repeated elements") {
    val a = List(4, 10, 3, 10, 2, 10, 1, 4)
    val b = List(1, 2, 3, 4, 4, 10, 10, 10)

    heap.add(a)
    val result = heap.sort()
    assert(result === b)
  }

  //  Test case: Single-element array
  test("Single-element array") {
    val a = List(5)

    heap.add(a)
    val result = heap.sort()
    assert(result === a)
  }

  //Test case: Empty arrangement
  test("Empty arrangement") {
    val a = List[Int]()

    heap.add(a)
    val result = heap.sort()
    assert(result.isEmpty)
  }

  // Test case: Arrangement with all elements the same
  test("Arrangement with all elements the same") {
    val a = List(5, 5, 5, 5, 5, 5, 5)

    heap.add(a)
    val result = heap.sort()
    assert(result === a)
  }
  
}
