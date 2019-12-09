import scala.collection.mutable.ArrayBuffer

object SortApplications {

  /** Intersection / join between two sorted arrays
   *  1. Brute-force: O(m *n)
   * + iterate through elements of one array
   * + compare vs elements of the other
   *  - improvement when one array reused for lookup :
   * + build hash-table of smaller array -> O(n) time cost
   * + O(1) lookup time
   *  2. Iterate through smaller array, do binary search on larger array
   * O (n * log(m)) if m > n
   *  3. "Merge join": simultaneously advancing both arrays
   *    - Picking up equal elements
   *    - Eliminate smaller element and advance the array owing it
   * */
  def sortedJoin(sortedA1: Array[Int], sortedA2: Array[Int]): Array[Int] = {
    var (i, j) = (0, 0)
    val intersection = ArrayBuffer.empty[Int]
    while (i < sortedA1.size && j < sortedA2.size) {
      if (sortedA1(i) == sortedA2(j) &&
        (i == 0 || !(sortedA1(i) == sortedA1(i - 1)))) { // for duplicated elements
        intersection += sortedA1(i)
        i += 1
        j += 1
      } else if (sortedA1(i) < sortedA2(j)) i += 1
      else j += 1
    }

    intersection.toArray
  }

  /**
   * Finding a pair in sorted array having sum as defined
   * 1. brute-force: for each element, go over the rest, take the sum and compare
   *  - O(n^2)
   *  2. For each element x , find the other element (sum-x) by binary search on the remaining list
   *  - O(n * log(n))
   *  3. Maintaing low & high index. Calculate sum of elements at low + high index
   *   - O(n) time
   * @param data
   * @param sum
   * @return
   */
  def hasPairWithSum(data: Array[Int], sum: Int):  Boolean = {
    var lo = 0
    var hi = data.size -1
    while (lo < hi ) {
      val sumGuess = data(lo) + data(hi)
      if (sumGuess == sum)  {
        println(s"Pair indexes ($lo, $hi) | pair number (${data(lo)}, ${data(hi)})")
        return true
      } else if (sumGuess > sum ) hi = hi - 1 // lower high index
      else  lo += 1  // higher low index
    }
  false
  }



  ///def trackSimultaneousEvents (a: Stream[Int]) = ???
}

import SortApplications._

val sorteda1 = Array(1, 4, 5, 6, 7, 7)
val sorteda2 = Array(2, 4, 6, 7, 9)
sortedJoin(sorteda1, sorteda2)

hasPairWithSum(sorteda1, 10)