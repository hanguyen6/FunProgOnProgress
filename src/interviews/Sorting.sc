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


}

import Sorting.sSort
val x = Array(1,10, 3, 2,4,6,2,3)
sSort(x)
