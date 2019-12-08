import scala.collection.mutable.ArrayBuffer
object SortApplications {

    /** Intersection / join between two sorted arrays
     *  1. Brute-force: O(m *n)
     *   + iterate through elements of one array
     *   + compare vs elements of the other
     *  - improvement when one array reused for lookup :
     *    + build hash-table of smaller array -> O(n) time cost
     *    + O(1) lookup time
     *  2. Iterate through smaller array, do binary search on larger array
     *  O (n * log(m)) if m > n
     *  3. "Merge join": simultaneously advancing both arrays
     *    - Picking up equal elements
     *    - Eliminate smaller element and advance the array owing it
     * */
    def sortedJoin(sortedA1: Array[Int], sortedA2: Array[Int]): Array[Int] = {
      var (i,j) = (0,0)
      val intersection = ArrayBuffer.empty[Int]
      while (i < sortedA1.size && j < sortedA2.size) {
        if (sortedA1(i) == sortedA2(j) &&
          (i==0 || !(sortedA1(i) == sortedA1(i-1)))) { // for duplicated elements
          intersection += sortedA1(i)
          i += 1
          j +=1
        } else if (sortedA1(i) < sortedA2(j)) i+=1
        else j+=1
      }

      intersection.toArray
    }
}

import SortApplications.sortedJoin

val sorteda1 = Array(1,3,4,6,7,7)
val sorteda2 = Array(2,4,6,7,9)
sortedJoin(sorteda1, sorteda2)