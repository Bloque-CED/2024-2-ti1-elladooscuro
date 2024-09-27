package co.com.u.icesi

import org.scalatest.funsuite.AnyFunSuite

class RadixSortTest extends AnyFunSuite {

  val radix = new RadixSort()

  //RadixSort -> del cap 8 text de Cormen
  test("An List of positive integers") {
    val input = List(170, 45, 75, 90, 802, 24, 2, 66)
    val expected = List(2, 24, 45, 66, 75, 90, 170, 802)
    assert(radix.sort(input) === expected)
  }

  test("An List with duplicates") {
    val input = List(170, 45, 75, 90, 802, 24, 75, 66)
    val expected = List(24, 45, 66, 75, 75, 90, 170, 802)
    assert(radix.sort(input) === expected)
  }

  test("An List with a single element") {
    val input = List(42)
    val expected = List(42)
    assert(radix.sort(input) === expected)
  }

  test("Already sorted List") {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(radix.sort(input) === expected)
  }

  test("An empty List") {
    val input = List[Int]()
    val expected = List[Int]()
    assert(radix.sort(input) === expected)
  }

}
