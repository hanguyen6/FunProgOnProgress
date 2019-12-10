import Sorting._
val x = Array(1,10, 3, 2,4,6,2,3)
sSort(x)

val y = Array(1,10, 3, 2,4,6,2,3)
val xs = y.toList
iSort(y)
iSortList(xs)

val z = Array(1,10, 3, 2,4,6,2,3)
mSort(z)

val t = Array(1,10, 3, 2,4,6,2,3)
qSort(t)

object Sorting {

  /**
    * Quick Sort: a divide , conquer & combine problem
    * In-place sorting
    * - Divide the array into two halves based on a pivot point
    *     - left sub array: elements smaller than pivot
    *     - right sub array: elements larger than pivot
    *  - Conquer by sorting sub arrays using qSort
    *     base case: sub array is sorted when having <= 1 element
    *  - Combine two sub arrays with pivot between
    *  Time Complexity:
    *    - linear time partitioning O(n)
    *    - worse case: un-balanced partitioning: one partition having no element while the other having the rest
    *           - total O(n) time for each level for two halves
    *           - n levels
    *           => O(n^2)
    *    - best case: balanced partitioning : two halves having similar elements
    *         - total O(n) time for each level for two halves
    *         - log2(n) levels
    *         => O(log2(n) * n)
    *    - average running time:
    *
    * @param a
    * @return
    */
  def qSort(a: Array[Int]): Array[Int]  = {
    if (a.size <=1) a
    else {
      //val pivot = a(a.size-1)
      val pivot = findPivot(a)
      val lowHalves = a.filter(pivot >)
      val highHalves = a.filter(pivot <)
      println(s"with pivot $pivot sorting two sub arrays: " +
        s"low  [${lowHalves.mkString(",")}] & " +
        s"high [${highHalves.mkString(",")}]")

      val combine = Array.concat(qSort(lowHalves)
                  ,a.filter(pivot==)
                  ,qSort(highHalves))
      println(s"combined sorted array: [${combine.mkString(",")}]")
      combine
    }
  }

  /**
   * Find good pivot to separate array into halves -> O(n) time
   * @param a
   * @return
   */
  def findPivot(a: Array[Int]): Int = {
    if (a.size <=5) {
      val sortedA = a.sorted
      sortedA(3)
    } else {
      def chunk(a: Array[Int], size: Int): List[Array[Int]] = ???
      val chunks = chunk(a, 5)
      val fullChunks = chunks.filter(chunk => chunk.size == 5 )
      val sortedFullChunks: List[Array[Int]] = fullChunks.map(chunk => chunk.sorted)
      val median_of_medians = sortedFullChunks.map(chunk => chunk(3))
      findPivot(median_of_medians.toArray)
    }

  }

  /** Merge Sort: a divide-conquer-combine problem
    * Not-in-place sorting
    * - Divide a array into halves with equal size
    * - Sort sub arrays
    * - Combine two arrays
    * Time Complexity: O(n * log2(n))
    *   - total linear time merging 0(n) for each level for two halves
    *   - log2(n) levels
    */
  def mSort(a: Array[Int]): Array[Int] = {
      if (a.size >= 2) println(s"Sorting [${a.mkString(",")}] ...")

    def combine(sortedA1: Array[Int], sortedA2: Array[Int]): Array[Int] = {
      if (sortedA1.isEmpty) sortedA2
        else if (sortedA2.isEmpty) sortedA1
        else {
          println(s"Combine 2 sorted sub arrays [${sortedA1.mkString(",")}] & [${sortedA2.mkString(",")}]")
          sortedA1.head <= sortedA2.head match {
            case true =>   Array(sortedA1.head) ++ combine(sortedA1.tail, sortedA2)
            case false =>  Array(sortedA2.head) ++ combine(sortedA1, sortedA2.tail)
          }

        }
      }

    if (a.size <=1) a
    else {
      val splitIdx = a.size /2
      val (lowHalves, rightHalves) =
        (a.slice(0, splitIdx), a.slice(splitIdx, a.size))
      println(s"Split [${a.mkString(",")}] into two sub arrays [${lowHalves.mkString(",")}] and [${rightHalves.mkString(" ")}] ")

      combine(mSort(lowHalves), mSort(rightHalves))

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

