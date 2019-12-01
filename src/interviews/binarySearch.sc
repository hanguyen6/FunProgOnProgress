object  binarySearch {

  /**
    * Binary Search on sorted array
    * @param t: number to search
    * @param a: sorted array
    * @return: index of the number
    */
  def bSearch(t: Int, a: Array[Int]): Int = {
    var (low, high) = (0, a.size-1)
    while (low <= high) {
      val middle = low + (high-low)/2
      if (a(middle) < t) low = middle + 1
      else if (a(middle) == t) middle
      else high = middle -1
    }
    -1
  }

  val a = Array(1,2,4,7,8,10,34, 405)
  val t = 7
  println(bSearch(t, a))

}


