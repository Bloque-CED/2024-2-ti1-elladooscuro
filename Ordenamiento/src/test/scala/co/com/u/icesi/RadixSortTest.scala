package co.com.u.icesi

import org.scalatest.funsuite.AnyFunSuite

class RadixSortTest {

  val radix = new RadixSort()

  //RadixSort -> del cap 8 text de Cormen
  test("An array of positive integers") {
    val input = Array(170, 45, 75, 90, 802, 24, 2, 66)
    val expected = Array(2, 24, 45, 66, 75, 90, 170, 802)
    assert(radix.radixSort(input) === expected)
  }

  test("An array with duplicates") {
    val input = Array(170, 45, 75, 90, 802, 24, 75, 66)
    val expected = Array(24, 45, 66, 75, 75, 90, 170, 802)
    assert(radix.radixSort(input) === expected)
  }

  test("An array with a single element") {
    val input = Array(42)
    val expected = Array(42)
    assert(radix.radixSort(input) === expected)
  }

  test("Already sorted array") {
    val input = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val expected = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(radix.radixSort(input) === expected)
  }

  test("An empty array") {
    val input = Array[Int]()
    val expected = Array[Int]()
    assert(radix.radixSort(input) === expected)
  }

}
