import Sorting._
val x = Array(1,10, 3, 2,4,6,2,3)
sSort(x)

val y = Array(1,10, 3, 2,4,6,2,3)
val xs = y.toList
iSort(y)
iSortList(xs)

object Sorting {

  /** Merge Sort: a divide-conquer-combine problem
    * Not-in-place sorting
    * - Divide a array into halves
    * - Sort sub arrays
    * - Combine two arrays
    */
  def mSort(a: Array[Int]): Array[Int] = {

    if (a.size <=1) a
    else {
      val splitIdx = a.size /2
      val (lowHalves, rightHalves) =
        (a.slice(0, splitIdx), a.slice(splitIdx+1, a.size))
      combine(mSort(lowHalves), mSort(rightHalves))

      def combine(sortedA1: Array[Int], sortedA2: Array[Int]): Array[Int] = {
        if (sortedA1.isEmpty) sortedA2
        else if (sortedA2.isEmpty) sortedA1
        else {
           Array(sortedA1.head min sortedA2.head) ++
        }
      }



    }
  }

  /**
    * Insertion Sort: in-place sorting
    * Construct a sorted sub array starting from one element from the array
    * Insert new element to the sorted array in the right place
    *   -
    * Time complexity:
    * ** best case: O(n) when putting element to the right most of sorted array
    * ** worst case  / random : O(n^2): when the array in reverse order and every elements need to be slide to the right
    * @param a unsorted array
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
      print(s"insert ${a(idx)} into [${a.slice(0,idx).mkString(",")}] --> ")
      insert(a(idx),  a, idx)
      println(s"${a.mkString(" ")}")
    }

    a
  }

  def iSortList(xs: List[Int]): List[Int] = xs match  {
    case x :: xs1 => insert(x, iSortList(xs1))
    case _ => xs
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match  {
    case x1 :: xs1 => {
      if (x < x1) x :: xs
      else x1 :: insert(x, xs1)
    }
    case _ => x :: xs
  }


  /**
    * Selection Sort: in-place sorting
    * Find the minimum value from the array
    * Put the value to top of the list
    * Time complexity: O(n^2)
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

