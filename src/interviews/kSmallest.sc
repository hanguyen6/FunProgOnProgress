import scala.collection.mutable
import scala.collection.mutable.PriorityQueue


object kSmallest {

  /**
   * Sort the list using mergeSort takes O(n*log(n)) time
   * Retrieve the k element take O(k) time if linked list and O(1) if arrayList
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
   * Using a max heap to keep k smallest number. Head is the k smallest element. Using O(k) memory
   * Insert n elements takes O(n*log(k)) time due to each iteration from n element, insert into heap
   * Retrieve k smallest element take O(1) time
   *
   * @param x
   * @param k
   * @return
   */
  def kSmallestV2(x: List[Int], k: Int): Int = {
    println(s"Creating a max heap size $k")
    val heap  = new mutable.PriorityQueue[Int]()
    for (elem <- x ) {
      if (heap.isEmpty || heap.size < k) {
        println(s"enqueue $elem")
        heap enqueue (elem)
      }
      else if (heap.head > elem) {
        println(s"dequeue ${heap dequeue()}")
        println(s"enqueue ${elem} ")
        heap enqueue (elem)
      }
      else println(s"$elem not match requirement !")
    }
    println(s"min heap of $k elements: ${heap.toString()}")
    heap.head
  }

  val x = List(1,2345, 10, 2, 6,3,5, 3234,5656, 7,10, 340, 123, 345, 453)

  println(kSmallest(x, 4))
  println(kSmallestV2(x, 4))
}