package co.com.u.icesi

class CountingSort {

  def sort(arr: List[Int]): List[Int] = {
    if (arr.isEmpty) return arr

    val (minValue, maxValue) = findMinMax(arr)
    val range = maxValue - minValue + 1

    val countArray = new Array[Int](range)
    fillCountArray(arr, countArray, minValue)

    createSortedList(countArray, minValue)
  }

  private def findMinMax(arr: List[Int]): (Int, Int) = arr match {
    case Nil => (0, 0)
    case head :: tail => findMinMaxRec(tail, head, head)
  }
  
  private def findMinMaxRec(arr: List[Int], min: Int, max: Int): (Int, Int) = arr match {
    case Nil => (min, max)
    case head :: tail =>
      val newMin = math.min(min, head)
      val newMax = math.max(max, head)
      findMinMaxRec(tail, newMin, newMax)
  }

  private def fillCountArray(arr: List[Int], countArray: Array[Int], minValue: Int): Unit = arr match {
    case Nil => () // No hacer nada si la lista está vacía
    case head :: tail =>
      val index = head - minValue
      countArray(index) = countArray(index) + 1
      fillCountArray(tail, countArray, minValue)
  }

  private def createSortedList(countArray: Array[Int], minValue: Int): List[Int] = {
    def createSortedListRec(index: Int, result: List[Int]): List[Int] = {
      if (index >= countArray.length) result
      else if (countArray(index) > 0) {
        val newResult = result ::: List.fill(countArray(index))(index + minValue)
        createSortedListRec(index + 1, newResult)
      } else {
        createSortedListRec(index + 1, result)
      }
    }

    createSortedListRec(0, List())
  }
}
