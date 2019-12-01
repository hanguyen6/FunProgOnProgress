object Sorting {

  /**
    * Selection Sort: Find the minimum value from the array
    * Put the value to top of the list
    * @param a : unsorted Array
    * @return sorted Array
    */
  def sSort(a: Array[Int]): Array[Int] = {

    def swap(a: Array[Int], fstIdx: Int, minIdx: Int): Unit = {
      val tmp = a(fstIdx)
      a(fstIdx) = a(minIdx)
      a(minIdx) = tmp
    }

    def indexOfSelection(a: Array[Int], startIdx: Int): Int = {
      var selectionValue = a(startIdx)
      var selectionIndex = startIdx

      for (idx <- selectionIndex + 1 until a.length) {

        if (selectionValue > a(idx)) {
            selectionValue = a(idx)
            selectionIndex = idx
          }
      }
      selectionIndex
    }


    for (x <- a ) {
      val minIndex = indexOfSelection(a, a.indexOf(x))
      swap(a, a.indexOf(x), minIndex)
    }

    a
  }

  /**
    * Insertion Sort: for each element, put the element to the sorted array on the left
    * @param a
    * @return
    */
  def iSort(a: Array[Int]): Array[Int] = {
    def insert(value: Int, sortedArray: Array[Int], rightIndex: Int): Unit = {
      if (sortedArray.isEmpty) Array(value)
      else {
        for (idx <- rightIndex until 0 by -1 if sortedArray(idx) > value) {
            sortedArray(idx+1) = sortedArray(idx)
            sortedArray(idx) = value
        }
      }
    }

    for (idx <- 0 until a.size) {
      println(s"insert ${a(idx)}")
      insert(a(idx),  a, idx)
    }

    a
  }


}

import Sorting._
val x = Array(1,10, 3, 2,4,6,2,3)
sSort(x)

val y = Array(1,10, 3, 2,4,6,2,3)
iSort(y)