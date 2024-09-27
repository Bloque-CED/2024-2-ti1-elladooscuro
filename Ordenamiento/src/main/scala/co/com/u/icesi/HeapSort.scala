package co.com.u.icesi

import scala.annotation.tailrec


class HeapSort {

  private var heap: List[Int] = List()

  def add(a: List[Int]): Unit = {
    heap = heap ::: a
    heap = heapifyUp(heap, heap.length - 1)
  }

  def add(element: Int): Unit = {
    heap = addToHeap(heap, element)
    heap = heapifyUp(heap, heap.length - 1)
  }

  def removeMax(): Option[Int] = heap match {
    case Nil => None
    case _ :: Nil =>
      val maxElement = heap.head
      heap = Nil
      Some(maxElement)
    case head :: tail =>
      val maxElement = heap.head
      heap = heap.last :: tail.init
      heap = heapifyDown(heap, 0)
      Some(maxElement)
  }

  def sort(): List[Int] = {
    var sortedHeap: List[Int] = List()
    val originalHeap = heap

    while (heap.nonEmpty) {
      removeMax() match {
        case Some(maxElement) => sortedHeap = maxElement :: sortedHeap
        case None =>
      }
    }

    heap = originalHeap
    sortedHeap
  }

  def isEmpty: Boolean = heap.isEmpty

  private def addToHeap(heap: List[Int], element: Int): List[Int] = heap match {
    case Nil => List(element)
    case _ => element :: heap.reverse match {
      case newHeap => newHeap.reverse
    }
  }

  @tailrec
  private def heapifyUp(heap: List[Int], index: Int): List[Int] = (heap, index) match {
    case (Nil, _) => heap
    case (_, 0) => heap
    case (list, _) =>
      val parentIndex = (index - 1) / 2
      if (list(index) > list(parentIndex)) {
        swap(heap, index, parentIndex) match {
          case swappedList => heapifyUp(swappedList, parentIndex)
        }
      } else list
  }

  @tailrec
  private def heapifyDown(heap: List[Int], index: Int): List[Int] = {
    val leftChildIndex = 2 * index + 1
    val rightChildIndex = 2 * index + 2

    (heap, leftChildIndex, rightChildIndex) match {
      case (Nil, _, _) => heap
      case (_, left, _) if left >= heap.length => heap
      case (_, left, right) if right >= heap.length =>
        if (heap(index) < heap(left)) {
          swap(heap, index, left) match {
            case swappedList => heapifyDown(swappedList, left)
          }
        } else heap
      case (_, left, right) =>
        val largerChildIndex = if (heap(left) > heap(right)) left else right
        if (heap(index) < heap(largerChildIndex)) {
          swap(heap, index, largerChildIndex) match {
            case swappedList => heapifyDown(swappedList, largerChildIndex)
          }
        } else heap
    }
  }

  private def swap(list: List[Int], i: Int, j: Int): List[Int] = {
    (list, i, j) match {
      case (Nil, _, _) => list
      case (_, x, y) if x == y => list
      case (lst, x, y) =>
        val (min, max) = if (x < y) (x, y) else (y, x)
        val (beforeMin, afterMin) = lst.splitAt(min)
        val (minElement, afterMax) = afterMin.splitAt(1)
        val (middle, maxElement :: rest) = afterMax.splitAt(max - min - 1)
        beforeMin ::: maxElement :: middle ::: minElement ::: rest
    }
  }

}
