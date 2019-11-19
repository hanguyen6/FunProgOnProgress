import scala.collection.mutable
import scala.collection.mutable.PriorityQueue


object kSmallest {

  /**
   * Sort the list using mergeSort takes O(n*log(n)) time
   * Retrieve the k element take O(k) time
   * => total time: O(n*log(n)) + O(k)
   * @param x
   * @param k
   * @return
   */
  def kSmallest(x: List[Int], k: Int): Int = {
    val sortedX = x.sorted
    sortedX(k-1)
  }

  /**
   * Using a min heap to keep k smallest number -> take O(k) memory space
   * Insert n elements takes O(n* log(n)) time due to reiterate n element + insert into heap
   * Retrieve k smallest element take O(1) time
   * => total time: O(n*log(n)) + O(1)
   * @param x
   * @param k
   * @return
   */
  def kSmallestV2(x: List[Int], k: Int): Int = {
    println(s"Creating a min heap size $k")
    val heap  = new mutable.PriorityQueue[Int]()
    for (elem <- x ) {
      if (heap.isEmpty || heap.size < k) heap enqueue (elem)
      else if (heap.head > elem) {
        heap dequeue()
        heap enqueue (elem)
      }
    }
    heap.head
  }

  val x = List(1,2345, 10, 3, 5,3,5, 3234,5656, 6,10, 340, 123, 345, 453)

  println(kSmallest(x, 2))
  println(kSmallestV2(x, 2))
}